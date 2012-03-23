package net.ohloh.ohcount4j.detect;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;
import static org.testng.AssertJUnit.assertEquals;

import java.io.IOException;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.SourceFile;

import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;

public class ExtnASXResolverTest {

	private ExtnASXResolver r;

	@BeforeTest()
	public void setup() {
		this.r = new ExtnASXResolver();
	}

	@Test
	public void canResolvetest() {
		assertFalse(r.canResolve(Language.RUBY));
		assertTrue(r.canResolve(Language.ASSEMBLY));
		assertTrue(r.canResolve(Language.XML));
	}

	@Test
	public void returnsAssemblyByDefaultTest() throws IOException {
		assertEquals(Language.ASSEMBLY, r.resolve(new SourceFile("foo.asx", "")));
	}

	@Test
	public void assemblyExample() throws IOException {
		assertEquals(Language.ASSEMBLY, r.resolve(new SourceFile("foo.asx",
				"\tORG $8000 ; comment\n")));

		assertEquals(Language.ASSEMBLY, r.resolve(new SourceFile("foo.asx",
				"; Comment with filename foo.asx\n")));
	}

	@Test
	public void xmlExample() throws IOException {
		assertEquals(Language.XML, r.resolve(new SourceFile("foo.asx",
				"<asx version=\"3.0\">")));

		assertEquals(Language.XML, r.resolve(new SourceFile("foo.asx",
				"  <ASX>")));
	}

}
