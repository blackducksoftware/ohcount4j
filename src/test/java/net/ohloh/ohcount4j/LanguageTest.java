package net.ohloh.ohcount4j;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;
import static org.testng.AssertJUnit.assertEquals;
import net.ohloh.ohcount4j.scan.AugeasScanner;
import net.ohloh.ohcount4j.scan.CStyleScanner;

import org.testng.Assert;
import org.testng.annotations.Test;

public class LanguageTest {

    @Test
    public void unameTest() {
        assertEquals("c", Language.C.uname());
        assertEquals("ruby", Language.RUBY.uname());

        // GoLang
        assertEquals("golang", Language.GOLANG.uname());

        // Augeas
        assertEquals("augeas", Language.AUGEAS.uname());
    }

    @Test
    public void extensionsTest() {
        assertTrue(Language.RUBY.getExtensions().contains("rb"));
        assertTrue(Language.RUBY.getExtensions().contains("ru"));
        assertFalse(Language.RUBY.getExtensions().contains("c"));

        // GoLang
        assertEquals(Language.GOLANG.getExtensions().size(), 1);
        assertEquals(Language.GOLANG.getExtensions().get(0), "go");

        // Augeas
        assertEquals(Language.AUGEAS.getExtensions().size(), 1);
        assertEquals(Language.AUGEAS.getExtensions().get(0), "aug");
    }

    @Test
    public void filenamesTest() {
        assertTrue(Language.RUBY.getFilenames().contains("Rakefile"));
        assertFalse(Language.RUBY.getFilenames().contains("Makefile"));
    }

    @Test
    public void testCategory() {
        Assert.assertEquals(Language.AUGEAS.category(), LanguageCategory.LOGIC);
        Assert.assertEquals(Language.GOLANG.category(), LanguageCategory.LOGIC);
    }

    @Test
    public void testScannerClass() {
        Assert.assertEquals(Language.AUGEAS.scannerClass(), AugeasScanner.class);
        Assert.assertEquals(Language.GOLANG.scannerClass(), CStyleScanner.class);
    }
}
