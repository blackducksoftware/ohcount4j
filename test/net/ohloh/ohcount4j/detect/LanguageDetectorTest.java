package net.ohloh.ohcount4j.detect;

import net.ohloh.ohcount4j.scan.CScanner;
import net.ohloh.ohcount4j.scan.RubyScanner;

import org.testng.annotations.Test;
import static org.testng.AssertJUnit.assertEquals;


public class LanguageDetectorTest {

	@Test
	public void testDetect() {
		assertEquals(null, LanguageNameDetector.detect(null));
		assertEquals(null, LanguageNameDetector.detect(""));
		assertEquals(null, LanguageNameDetector.detect("not_a_language_name"));

		assertEquals(RubyScanner.class, LanguageNameDetector.detect("ruby"));
		assertEquals(RubyScanner.class, LanguageNameDetector.detect("Ruby"));
		assertEquals(CScanner.class, LanguageNameDetector.detect("c"));
		assertEquals(CScanner.class, LanguageNameDetector.detect("C"));
	}
}
