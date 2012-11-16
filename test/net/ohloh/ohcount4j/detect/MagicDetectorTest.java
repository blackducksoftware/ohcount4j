package net.ohloh.ohcount4j.detect;

import static org.testng.AssertJUnit.assertEquals;
import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.OhcountException;

import org.testng.annotations.Test;

public class MagicDetectorTest {

    @Test
    public void getLanguageNameTest() throws OhcountException {
        assertEquals(null, MagicDetector.getLanguageName(null));
        assertEquals(null, MagicDetector.getLanguageName(""));
        assertEquals(null, MagicDetector.getLanguageName("mysterious unexpected description"));

        assertEquals("ruby", MagicDetector.getLanguageName("a ruby script text executable"));
        assertEquals("Java", MagicDetector.getLanguageName("ASCII Java program text"));
    }

    @Test
    public void detectTest() throws OhcountException {
        assertEquals(null, MagicDetector.detect(null));
        assertEquals(null, MagicDetector.detect(""));
        assertEquals(null, MagicDetector.detect("#!/usr/bin/env unknown_language\n"));

        assertEquals(Language.RUBY, MagicDetector.detect("#!/usr/bin/env ruby\n"));
    }
}
