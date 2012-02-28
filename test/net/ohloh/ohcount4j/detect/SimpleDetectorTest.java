package net.ohloh.ohcount4j.detect;

import net.ohloh.ohcount4j.io.InputStreamBlob;
import net.ohloh.ohcount4j.scan.*;

import org.testng.annotations.Test;
import static org.testng.AssertJUnit.*;

public class SimpleDetectorTest {

	@Test
	public void basic() {
		assertDetect("main.c", CScanner.class);
		assertDetect("main.css", CSSScanner.class);
		assertDetect("main.htm", HTMLScanner.class);
		assertDetect("main.html", HTMLScanner.class);
		assertDetect("main.java", JavaScanner.class);
		assertDetect("main.js", JavaScriptScanner.class);
		assertDetect("Makefile", MakefileScanner.class);
		assertDetect("main.rb", RubyScanner.class);
		assertDetect("config.ru", RubyScanner.class);
		assertDetect("Gemfile", RubyScanner.class);
		assertDetect("Rakefile", RubyScanner.class);
	}

	protected void assertDetect(String filename, Class<?> c) {
		assertEquals(c, SimpleDetector.detect(new InputStreamBlob(filename, System.in)).getClass());
	}
}