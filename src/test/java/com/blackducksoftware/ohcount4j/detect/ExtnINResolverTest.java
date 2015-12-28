package com.blackducksoftware.ohcount4j.detect;

import static org.testng.Assert.assertTrue;
import static org.testng.AssertJUnit.assertEquals;

import java.io.IOException;

import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.SourceFile;

public class ExtnINResolverTest {

    private ExtnINResolver r;

    @BeforeTest()
    public void setup() {
        r = new ExtnINResolver();
    }

    @Test
    public void canResolvetest() {
        for (Language l : Language.values()) {
            assertTrue(r.canResolve(l));
        }
    }

    @Test
    public void unknownByDefaultTest() throws IOException {
        assertEquals(Language.UNKNOWN, r.resolve(new SourceFile("foo.in", "")));
    }

    @Test
    public void strippedPathTest() {
        assertEquals("foo", r.strippedPath("foo"));
        assertEquals("foo", r.strippedPath("foo.in"));
        assertEquals("foo.c", r.strippedPath("foo.c"));
        assertEquals("foo.c", r.strippedPath("foo.c.in"));
    }

    @Test
    public void cExample() throws IOException {
        assertEquals(Language.C, r.resolve(new SourceFile("foo.h.in", "#include <stdio.h>")));
    }

    @Test
    public void cppExample() throws IOException {
        assertEquals(Language.CPP, r.resolve(new SourceFile("foo.h.in", "#include <string>")));
    }

    @Test
    public void unknownExample() throws IOException {
        assertEquals(Language.UNKNOWN, r.resolve(new SourceFile("foo.in", "# unknown")));
    }
}
