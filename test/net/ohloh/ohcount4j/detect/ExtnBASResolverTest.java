package net.ohloh.ohcount4j.detect;

import net.ohloh.ohcount4j.Language;
import static org.testng.AssertJUnit.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

import java.io.IOException;
import java.util.ArrayList;

import net.ohloh.ohcount4j.SourceFile;

import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;

public class ExtnBASResolverTest {

	private ExtnBASResolver r;

	@BeforeTest()
	public void setup() {
		this.r = new ExtnBASResolver();
	}

	@Test
	public void canResolvetest() {
		assertFalse(r.canResolve(Language.RUBY));
		assertTrue(r.canResolve(Language.CLASSIC_BASIC));
		assertTrue(r.canResolve(Language.STRUCTURED_BASIC));
		assertTrue(r.canResolve(Language.VB));
	}

	@Test
	public void structuredBasicByDefaultTest() throws IOException {
		assertEquals(Language.STRUCTURED_BASIC, r.resolve(new SourceFile("foo.bas", "")));
	}

	@Test
	public void classicBasicExamplesTest() throws IOException {
		assertEquals(Language.CLASSIC_BASIC, r.resolve(new SourceFile("foo.bas", "10 PRINT 'HELLO WORLD'")));
		assertEquals(Language.CLASSIC_BASIC, r.resolve(new SourceFile("foo.bas", "  100 REM")));
	}

	@Test
	public void structuredBasicExamplesTest() throws IOException {
		assertEquals(Language.STRUCTURED_BASIC, r.resolve(new SourceFile("foo.bas", "PRINT 'HELLO WORLD'")));
		assertEquals(Language.STRUCTURED_BASIC, r.resolve(new SourceFile("foo.bas", "    REM")));
	}

	@Test
	public void notVisualBasicTest() throws IOException {
		// Technically, these *are* Visual Basic.
		// However the resolver relies on the names of surrounding files to know this.
		// We will pass only *.bas files so that Visual Basic will not be detected.
		ArrayList<String> filenames = new ArrayList<String>();
		filenames.add("foo.bas");
		filenames.add("bar.bas");

		assertEquals(Language.STRUCTURED_BASIC,
				r.resolve(new SourceFile("foo.bas", "Sub Main()"), filenames));
		assertEquals(Language.STRUCTURED_BASIC,
				r.resolve(new SourceFile("foo.bas", "Public Class Foo"), filenames));
	}

	@Test
	public void visualBasicTest() throws IOException {
		ArrayList<String> filenames = new ArrayList<String>();
		filenames.add("foo.bas");
		filenames.add("bar.frx");

		assertEquals(Language.VB,
				r.resolve(new SourceFile("foo.bas", "Sub Main()"), filenames));
		assertEquals(Language.VB,
				r.resolve(new SourceFile("foo.bas", "Public Class Foo"), filenames));
	}
}