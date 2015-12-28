package com.blackducksoftware.ohcount4j.detect;

import static org.testng.AssertJUnit.assertEquals;

import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.Language;

public class EmacsModeDetectorTest {

    @Test
    public void testDetect() {
        assertEquals(null, EmacsModeDetector.detect(null));
        assertEquals(null, EmacsModeDetector.detect(""));
        assertEquals(null, EmacsModeDetector.detect("# -*- mode: not_a_language_name -*-"));

        assertEquals(Language.RUBY, EmacsModeDetector.detect("# -*- mode: Ruby -*-"));
        assertEquals(Language.C, EmacsModeDetector.detect("/* -*- mode: C; -*- */"));
    }

    @Test
    public void testGetMode() {
        assertMode(null, null);
        assertMode(null, "");
        assertMode(null, "/* */");
        assertMode(null, "/* -*- -*- */");
        assertMode("python", "# -*- python -*-");
        assertMode("F90", "! -*- F90 -*-");
        assertMode("C++", "/* -*- C++ -*- */");
        assertMode("C", "/* -*- mode: C; -*-");
        assertMode("TCL", "# -*-Mode: TCL;-*-");
    }

    protected void assertMode(String expected, String buffer) {
        assertEquals(expected, EmacsModeDetector.getMode(buffer));
    }
}
