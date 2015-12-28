package com.blackducksoftware.ohcount4j.detect;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;
import static org.testng.AssertJUnit.assertEquals;

import java.io.IOException;

import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.SourceFile;

public class ExtnINCResolverTest {

    private ExtnINCResolver r;

    @BeforeTest()
    public void setup() {
        r = new ExtnINCResolver();
    }

    @Test
    public void canResolvetest() {
        assertFalse(r.canResolve(Language.RUBY));
        assertTrue(r.canResolve(Language.PHP));
        assertTrue(r.canResolve(Language.BINARY));
    }

    @Test
    // With no other clues, the resolver should pick BINARY by default
    public void returnsBinaryByDefaultTest() throws IOException {
        assertEquals(Language.BINARY, r.resolve(new SourceFile("main.h", "")));
    }

    @Test
    public void phpExample() throws IOException {
        assertEquals(Language.PHP, r.resolve(new SourceFile("foo.inc",
                "<?php\n" +
                        "  // comment\n" +
                        "?>\n")));
    }

    @Test
    public void binaryExample() throws IOException {
        assertEquals(Language.BINARY, r.resolve(new SourceFile("foo.inc", "\u0000")));
    }
}
