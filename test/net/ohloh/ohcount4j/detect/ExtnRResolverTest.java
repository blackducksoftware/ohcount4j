package net.ohloh.ohcount4j.detect;

import net.ohloh.ohcount4j.Language;
import static org.testng.AssertJUnit.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

import java.io.IOException;

import net.ohloh.ohcount4j.SourceFile;

import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;

public class ExtnRResolverTest {

	private ExtnRResolver r;

	@BeforeTest()
	public void setup() {
		this.r = new ExtnRResolver();
	}

	@Test
	public void canResolvetest() {
		assertFalse(r.canResolve(Language.RUBY));
		assertTrue(r.canResolve(Language.R));
		assertTrue(r.canResolve(Language.REBOL));
	}

	@Test
	public void rByDefaultTest() throws IOException {
		assertEquals(Language.R, r.resolve(new SourceFile("foo.r", "")));
	}

	@Test
	public void rExampleTest() throws IOException {
		assertEquals(Language.R, r.resolve(new SourceFile("foo.r", "library(foo)")));
		assertEquals(Language.R, r.resolve(new SourceFile("foo.r", "f <- 0 # comment")));
	}

	@Test
	public void rebolExampleTest() throws IOException {
		assertEquals(Language.REBOL, r.resolve(new SourceFile("foo.r", "REBOL[] ; comment")));
		assertEquals(Language.REBOL, r.resolve(new SourceFile("foo.r", "rebol[] ; comment")));
	}

}