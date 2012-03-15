package net.ohloh.ohcount4j;

import org.testng.annotations.Test;
import static org.testng.AssertJUnit.assertEquals;

import static net.ohloh.ohcount4j.Language.*;

public class LanguageTest {

	@Test
	public void fromExtensionTest() {
		assertEquals(LANG_C, Language.fromExtension("c"));
		assertEquals(LANG_RUBY, Language.fromExtension("rb"));
	}
}
