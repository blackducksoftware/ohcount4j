package com.blackducksoftware.ohcount4j.detect;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;
import static org.testng.AssertJUnit.assertEquals;

import java.io.IOException;

import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.SourceFile;

public class ExtnRResolverTest {

    private ExtnRResolver r;

    @BeforeTest()
    public void setup() {
        r = new ExtnRResolver();
    }

    @Test
    public void canResolvetest() {
        assertFalse(r.canResolve(Language.RUBY));
        assertTrue(r.canResolve(Language.R));
        assertTrue(r.canResolve(Language.REBOL));
    }

    @Test
    public void rByDefaultTest() throws IOException {
        assertEquals(Language.R, r.resolve(new SourceFile("foo.r", "")));
    }

    @Test
    public void rExampleTest() throws IOException {
        assertEquals(Language.R, r.resolve(new SourceFile("foo.r", "library(foo)")));
        assertEquals(Language.R, r.resolve(new SourceFile("foo.r", "f <- 0 # comment")));
    }

    @Test
    public void rebolExampleTest() throws IOException {
        assertEquals(Language.REBOL, r.resolve(new SourceFile("foo.r", "REBOL[] ; comment")));
        assertEquals(Language.REBOL, r.resolve(new SourceFile("foo.r", "rebol[] ; comment")));
    }

}
