package net.ohloh.ohcount4j;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;
import static org.testng.AssertJUnit.assertEquals;

import org.testng.annotations.Test;

public class LanguageTest {

    @Test
    void unameTest() {
        assertEquals("c", Language.C.uname());
        assertEquals("ruby", Language.RUBY.uname());

        // GoLang
        assertEquals("GoLang", Language.GO_LANG.uname());
    }

    @Test
    void extensionsTest() {
        assertTrue(Language.RUBY.getExtensions().contains("rb"));
        assertTrue(Language.RUBY.getExtensions().contains("ru"));
        assertFalse(Language.RUBY.getExtensions().contains("c"));

        // GoLang
        assertEquals(Language.GO_LANG.getExtensions().size(), 1);
        assertEquals(Language.GO_LANG.getExtensions().get(0), "go");
    }

    @Test
    void filenamesTest() {
        assertTrue(Language.RUBY.getFilenames().contains("Rakefile"));
        assertFalse(Language.RUBY.getFilenames().contains("Makefile"));
    }
}
