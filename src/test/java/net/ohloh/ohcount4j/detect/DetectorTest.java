package net.ohloh.ohcount4j.detect;

import static net.ohloh.ohcount4j.Language.AUGEAS;
import static net.ohloh.ohcount4j.Language.AUTOCONF;
import static net.ohloh.ohcount4j.Language.AUTOMAKE;
import static net.ohloh.ohcount4j.Language.AWK;
import static net.ohloh.ohcount4j.Language.BFPP;
import static net.ohloh.ohcount4j.Language.BRAINFUCK;
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
import java.util.Arrays;
import java.util.List;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.OhcountException;
import net.ohloh.ohcount4j.SourceFile;

import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

public class DetectorTest {

    @Test(dataProvider = "detectByExtensions")
    public void detectByExtensionTest(Language language, List<String> fileNames) throws IOException {
        for (String fileName : fileNames) {
            assertDetect(fileName, language);
        }
    }

    @DataProvider
    public Object[][] detectByExtensions() {
        return new Object[][] {
                { C, Arrays.asList("main.c") },
                { CSS, Arrays.asList("main.css") },
                { HTML, Arrays.asList("main.htm", "main.html") },
                { JAVA, Arrays.asList("main.java") },
                { JAVASCRIPT, Arrays.asList("main.js") },
                { RUBY, Arrays.asList("main.rb", "config.ru") },
                { AUTOMAKE, Arrays.asList("make.am", "make.AM") },
                { AUTOCONF, Arrays.asList("configuration.ac", "configuration.autoconf") },
                { GOLANG, Arrays.asList("main.go") },
                { AUGEAS, Arrays.asList("main.aug") },
                { AWK, Arrays.asList("main.awk") },
                { BRAINFUCK, Arrays.asList("main.bf") },
                { BFPP, Arrays.asList("main.bfpp") }
        };
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
        assertFalse(Detector.getInstance().isBinary("awk"));

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
