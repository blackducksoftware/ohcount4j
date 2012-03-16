package net.ohloh.ohcount4j.detect;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.OhcountException;
import net.ohloh.ohcount4j.detect.Detector;
import net.ohloh.ohcount4j.io.SourceBuffer;
import static net.ohloh.ohcount4j.Language.*;

import org.testng.annotations.Test;
import static org.testng.AssertJUnit.*;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

public class DetectorTest {

	@Test
	public void basic() throws OhcountException {
		assertDetect("main.c",    C);
		assertDetect("main.css",  CSS);
		assertDetect("main.htm",  HTML);
		assertDetect("main.html", HTML);
		assertDetect("main.java", JAVA);
		assertDetect("main.js",   JAVASCRIPT);
		assertDetect("Makefile",  MAKE);
		assertDetect("main.rb",   RUBY);
		assertDetect("config.ru", RUBY);
		assertDetect("Gemfile",   RUBY);
		assertDetect("Rakefile",  RUBY);
	}

	protected void assertDetect(String filename, Language language) throws OhcountException {
		assertEquals(language, Detector.detect(new SourceBuffer(filename, "")));
	}

	@Test
	public void isBinaryTest() {
		assertFalse(Detector.isBinary(""));
		assertFalse(Detector.isBinary("txt"));

		assertTrue(Detector.isBinary("jpg"));
		assertTrue(Detector.isBinary("JPG"));
	}

	@Test
	public void getResolverTest() throws OhcountException {
		//assertEquals(null, Detector.getResolver(""));
		//assertEquals(null, Detector.getResolver("notfound"));

		assertTrue(Detector.getResolver("h") instanceof ExtnHResolver);
	}
}