package net.ohloh.ohcount4j;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;
import static org.testng.AssertJUnit.assertEquals;

import org.testng.annotations.Test;

public class LanguageTest {

    @Test
    public void unameTest() {
        assertEquals("c", Language.C.uname());
        assertEquals("ruby", Language.RUBY.uname());

        // GoLang
        assertEquals("golang", Language.GOLANG.uname());
    }

    @Test
    public void extensionsTest() {
        assertTrue(Language.RUBY.getExtensions().contains("rb"));
        assertTrue(Language.RUBY.getExtensions().contains("ru"));
        assertFalse(Language.RUBY.getExtensions().contains("c"));

        // GoLang
        assertEquals(Language.GOLANG.getExtensions().size(), 1);
        assertEquals(Language.GOLANG.getExtensions().get(0), "go");
    }

    @Test
    public void filenamesTest() {
        assertTrue(Language.RUBY.getFilenames().contains("Rakefile"));
        assertFalse(Language.RUBY.getFilenames().contains("Makefile"));
    }
}
