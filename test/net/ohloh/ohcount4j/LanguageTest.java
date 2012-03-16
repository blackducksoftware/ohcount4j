package net.ohloh.ohcount4j;

import org.testng.annotations.Test;
import static org.testng.AssertJUnit.assertEquals;

import net.ohloh.ohcount4j.Language;

public class LanguageTest {

	@Test
	public void fromExtensionTest() {
		assertEquals(Language.C, Language.fromExtension("c"));
		assertEquals(Language.RUBY, Language.fromExtension("rb"));
	}

	@Test void unameTest() {
		assertEquals("c", Language.C.uname());
		assertEquals("ruby", Language.RUBY.uname());
	}
}