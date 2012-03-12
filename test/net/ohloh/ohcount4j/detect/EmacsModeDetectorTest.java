package net.ohloh.ohcount4j.detect;

import net.ohloh.ohcount4j.scan.CScanner;
import net.ohloh.ohcount4j.scan.RubyScanner;

import org.testng.annotations.Test;
import static org.testng.AssertJUnit.assertEquals;

public class EmacsModeDetectorTest {

	@Test
	public void testDetect() {
		assertEquals(null, EmacsModeDetector.detect(null));
		assertEquals(null, EmacsModeDetector.detect(""));
		assertEquals(null, EmacsModeDetector.detect("# -*- mode: not_a_language_name -*-"));

		assertEquals(RubyScanner.class, EmacsModeDetector.detect("# -*- mode: Ruby -*-"));
		assertEquals(CScanner.class,    EmacsModeDetector.detect("/* -*- mode: C; -*- */"));
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