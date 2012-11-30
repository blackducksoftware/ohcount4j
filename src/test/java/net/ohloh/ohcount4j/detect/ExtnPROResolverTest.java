package net.ohloh.ohcount4j.detect;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;
import static org.testng.AssertJUnit.assertEquals;

import java.io.IOException;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.SourceFile;

import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;

public class ExtnPROResolverTest {

    private ExtnPROResolver r;

    @BeforeTest()
    public void setup() {
        r = new ExtnPROResolver();
    }

    @Test
    public void canResolvetest() {
        assertFalse(r.canResolve(Language.RUBY));
        assertTrue(r.canResolve(Language.MAKE));
        assertTrue(r.canResolve(Language.PVWAVE));
    }

    @Test
    public void returnsPvWaveByDefaultTest() throws IOException {
        assertEquals(Language.PVWAVE, r.resolve(new SourceFile("foo.pro", "")));
    }

    @Test
    public void qmakeExample() throws IOException {
        assertEquals(Language.MAKE, r.resolve(new SourceFile("foo.pro",
                "\tSOURCES += foo\n")));

        assertEquals(Language.MAKE, r.resolve(new SourceFile("foo.pro",
                "SOURCES+=FOO\n")));

        assertEquals(Language.MAKE, r.resolve(new SourceFile("foo.pro",
                " \t  CONFIG += foo\n")));

        assertEquals(Language.MAKE, r.resolve(new SourceFile("foo.pro",
                "CONFIG+=foo\n")));
    }

    @Test
    public void pvWaveExample() throws IOException {
        assertEquals(Language.PVWAVE, r.resolve(new SourceFile("foo.pro",
                "pro foo\n\n    print, 'hello world'\nend\n\n")));

        assertEquals(Language.PVWAVE, r.resolve(new SourceFile("foo.pro",
                "; comment")));
    }

}
