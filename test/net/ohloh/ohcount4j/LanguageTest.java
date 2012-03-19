package net.ohloh.ohcount4j;

import org.testng.annotations.Test;
import static org.testng.AssertJUnit.assertEquals;
import static org.testng.Assert.assertTrue;
import static org.testng.Assert.assertFalse;

import net.ohloh.ohcount4j.Language;

public class LanguageTest {

	@Test
	void unameTest() {
		assertEquals("c", Language.C.uname());
		assertEquals("ruby", Language.RUBY.uname());
	}

	@Test
	void extensionsTest() {
		assertTrue(Language.RUBY.getExtensions().contains("rb"));
		assertTrue(Language.RUBY.getExtensions().contains("ru"));
		assertFalse(Language.RUBY.getExtensions().contains("c"));
	}

	@Test
	void filenamesTest() {
		assertTrue(Language.RUBY.getFilenames().contains("Rakefile"));
		assertFalse(Language.RUBY.getFilenames().contains("Makefile"));
	}
}