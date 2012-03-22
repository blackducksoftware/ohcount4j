package net.ohloh.ohcount4j.detect;

import net.ohloh.ohcount4j.Language;
import static org.testng.AssertJUnit.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

import java.io.IOException;

import net.ohloh.ohcount4j.SourceFile;

import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;

public class ExtnINCResolverTest {

	private ExtnINCResolver r;

	@BeforeTest()
	public void setup() {
		this.r = new ExtnINCResolver();
	}

	@Test
	public void canResolvetest() {
		assertFalse(r.canResolve(Language.RUBY));
		assertTrue(r.canResolve(Language.PHP));
		assertTrue(r.canResolve(Language.BINARY));
	}

	@Test
	// With no other clues, the resolver should pick BINARY by default
	public void returnsBinaryByDefaultTest() throws IOException {
		assertEquals(Language.BINARY, r.resolve(new SourceFile("main.h", "")));
	}

	@Test
	public void phpExample() throws IOException {
		assertEquals(Language.PHP, r.resolve(new SourceFile("foo.inc",
				"<?php\n" +
				"  // comment\n" +
				"?>\n" )));
	}

	@Test
	public void binaryExample() throws IOException {
		assertEquals(Language.BINARY, r.resolve(new SourceFile("foo.inc", "\u0000" )));
	}
}