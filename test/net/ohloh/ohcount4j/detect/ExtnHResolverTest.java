package net.ohloh.ohcount4j.detect;

import net.ohloh.ohcount4j.Language;
import static org.testng.AssertJUnit.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

import java.io.IOException;
import java.util.Arrays;

import net.ohloh.ohcount4j.SourceFile;

import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;

public class ExtnHResolverTest {

	private ExtnHResolver r;

	@BeforeTest()
	public void setup() {
		this.r = new ExtnHResolver();
	}

	@Test
	public void canResolvetest() {
		assertFalse(r.canResolve(Language.RUBY));
		assertTrue(r.canResolve(Language.C));
		assertTrue(r.canResolve(Language.CPP));
		assertTrue(r.canResolve(Language.OBJECTIVE_C));
	}

	@Test
	// With no other clues, the resolver should pick C by default
	public void returnsCByDefaultTest() throws IOException {
		assertEquals(Language.C, r.resolve(new SourceFile("main.h", "")));
	}

	@Test
	public void findIncludesTest() throws IOException {
		SourceFile s;

		s = new SourceFile("main.h", "");
		assertEquals(0, r.findIncludes(s).size());

		s = new SourceFile("main.h", "#include <stdio.h>");
		assertEquals(1, r.findIncludes(s).size());
		assertTrue(r.findIncludes(s).contains("stdio.h"));

		s = new SourceFile("main.h", "#include \"stdio.h\"");
		assertEquals(1, r.findIncludes(s).size());
		assertTrue(r.findIncludes(s).contains("stdio.h"));

		s = new SourceFile("main.h",
				"/* Longer Example */\n" +
				"#include \"stdio.h\"\n" +
				"\n" +
				"#include <string.h>\n" +
				"#include <cassert>\n" +
				"//#include <foo.h>\n" +
				"\n" +
				"int main() {" +
				"  char *foo = \"#include <bar.h>\";\n" +
				")\n"
			);
		assertEquals(3, r.findIncludes(s).size());
		assertFalse(r.findIncludes(s).contains("stdio"));
		assertFalse(r.findIncludes(s).contains("string"));
		assertFalse(r.findIncludes(s).contains("foo"));
		assertFalse(r.findIncludes(s).contains("foo.h"));
		assertFalse(r.findIncludes(s).contains("bar"));
		assertFalse(r.findIncludes(s).contains("bar.h"));

		assertTrue(r.findIncludes(s).contains("stdio.h"));
		assertTrue(r.findIncludes(s).contains("string.h"));
		assertTrue(r.findIncludes(s).contains("cassert"));
	}

	@Test
	public void detectByIncludesTest() throws IOException {
		SourceFile s;

		s = new SourceFile("main.h", "#include <foo.h>");
		assertEquals(Language.C, r.resolve(s));

		s = new SourceFile("main.h", "#include <string.h>");
		assertEquals(Language.C, r.resolve(s));

		s = new SourceFile("main.h", "#include <string>");
		assertEquals(Language.CPP, r.resolve(s));

		s = new SourceFile("main.h", "#include <string.h>\n#include<string>\n");
		assertEquals(Language.CPP, r.resolve(s));

		s = new SourceFile("main.h", "#include <tr1/memory>");
		assertEquals(Language.CPP, r.resolve(s));
	}

	@Test
	public void detectByKeywordsTest() throws IOException {
		SourceFile s;

		s = new SourceFile("main.h", "namespace foo\n");
		assertEquals(Language.CPP, r.resolve(s));

		s = new SourceFile("main.h",
				"/* Multiline example */\n" +
				"namespace foo {\n" +
				"    template <typename> struct Foo;\n" +
				"}\n"
		);
		assertEquals(Language.CPP, r.resolve(s));
	}

	@Test
	public void detectObjectiveCTest() throws IOException {
		// No filenames given -> default result
		assertEquals(Language.C, r.resolve(new SourceFile("foo.h", "")));

		// Header has a corresponding *.c file
		assertEquals(Language.C,
			r.resolve(new SourceFile("foo.h", ""), Arrays.asList("foo.c")));

		// Header has a corresponding *.m file
		assertEquals(Language.OBJECTIVE_C,
			r.resolve(new SourceFile("foo.h", ""), Arrays.asList("foo.m")));

		// Header has a corresponding *.m file, this time with directory path
		assertEquals(Language.OBJECTIVE_C,
			r.resolve(new SourceFile("src/foo/foo.h", ""), Arrays.asList("src/foo/foo.m")));

		// The *.m file is in another directory, so we do not see it.
		assertEquals(Language.C,
			r.resolve(new SourceFile("include/foo.h", ""), Arrays.asList("src/foo.m")));
	}
}