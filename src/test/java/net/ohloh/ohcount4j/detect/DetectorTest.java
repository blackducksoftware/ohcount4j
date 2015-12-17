package net.ohloh.ohcount4j.detect;

import static net.ohloh.ohcount4j.Language.AUTOMAKE;
import static net.ohloh.ohcount4j.Language.C;
import static net.ohloh.ohcount4j.Language.CSS;
import static net.ohloh.ohcount4j.Language.GOLANG;
import static net.ohloh.ohcount4j.Language.HTML;
import static net.ohloh.ohcount4j.Language.JAVA;
import static net.ohloh.ohcount4j.Language.JAVASCRIPT;
import static net.ohloh.ohcount4j.Language.MAKE;
import static net.ohloh.ohcount4j.Language.RUBY;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;
import static org.testng.AssertJUnit.assertEquals;

import java.io.IOException;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.OhcountException;
import net.ohloh.ohcount4j.SourceFile;

import org.testng.annotations.Test;

public class DetectorTest {

    @Test
    public void detectByExtensionTest() throws IOException {
        assertDetect("main.c", C);
        assertDetect("main.css", CSS);
        assertDetect("main.htm", HTML);
        assertDetect("main.html", HTML);
        assertDetect("main.java", JAVA);
        assertDetect("main.js", JAVASCRIPT);
        assertDetect("main.rb", RUBY);
        assertDetect("config.ru", RUBY);
        assertDetect("make.am", AUTOMAKE);
        assertDetect("make.AM", AUTOMAKE);
        assertDetect("main.go", GOLANG);
    }

    @Test
    public void detectByFilenameTest() throws IOException {
        assertDetect("Makefile", MAKE);
        assertDetect("Gemfile", RUBY);
        assertDetect("Rakefile", RUBY);
    }

    protected void assertDetect(String filename, Language language) throws IOException {
        assertEquals(language, Detector.detect(new SourceFile(filename, "")));
    }

    @Test
    public void isBinaryTest() {
        assertFalse(Detector.getInstance().isBinary(""));
        assertFalse(Detector.getInstance().isBinary("txt"));
        assertFalse(Detector.getInstance().isBinary("am"));

        assertTrue(Detector.getInstance().isBinary("jpg"));
        assertTrue(Detector.getInstance().isBinary("JPG"));
    }

    @Test
    public void getResolverTest() throws OhcountException {
        assertTrue(Detector.getResolver("h") instanceof ExtnHResolver);
        assertTrue(Detector.getResolver("m") instanceof ExtnMResolver);
        assertTrue(Detector.getResolver("inc") instanceof ExtnINCResolver);
        assertTrue(Detector.getResolver("pp") instanceof ExtnPPResolver);
        assertTrue(Detector.getResolver("aspx") instanceof ExtnASPXResolver);
        assertTrue(Detector.getResolver("asx") instanceof ExtnASXResolver);

        assertTrue(Detector.getResolver("f") instanceof FortranResolver);
        assertTrue(Detector.getResolver("f90") instanceof FortranResolver);
    }
}
