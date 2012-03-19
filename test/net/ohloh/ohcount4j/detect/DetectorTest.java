package net.ohloh.ohcount4j.detect;

import java.io.IOException;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.OhcountException;
import net.ohloh.ohcount4j.detect.Detector;
import net.ohloh.ohcount4j.SourceFile;
import static net.ohloh.ohcount4j.Language.*;

import org.testng.annotations.Test;
import static org.testng.AssertJUnit.*;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

public class DetectorTest {

	@Test
	public void detectByExtensionTest() throws IOException {
		assertDetect("main.c",    C);
		assertDetect("main.css",  CSS);
		assertDetect("main.htm",  HTML);
		assertDetect("main.html", HTML);
		assertDetect("main.java", JAVA);
		assertDetect("main.js",   JAVASCRIPT);
		assertDetect("main.rb",   RUBY);
		assertDetect("config.ru", RUBY);
	}

	@Test
	public void detectByFilenameTest() throws IOException {
		assertDetect("Makefile",  MAKE);
		assertDetect("Gemfile",   RUBY);
		assertDetect("Rakefile",  RUBY);
	}

	protected void assertDetect(String filename, Language language) throws IOException {
		assertEquals(language, Detector.detect(new SourceFile(filename, "")));
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