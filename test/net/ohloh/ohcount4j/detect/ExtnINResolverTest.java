package net.ohloh.ohcount4j.detect;

import net.ohloh.ohcount4j.Language;
import static org.testng.AssertJUnit.assertEquals;
import static org.testng.Assert.assertTrue;

import java.io.IOException;

import net.ohloh.ohcount4j.SourceFile;

import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;

public class ExtnINResolverTest {

	private ExtnINResolver r;

	@BeforeTest()
	public void setup() {
		this.r = new ExtnINResolver();
	}

	@Test
	public void canResolvetest() {
		for (Language l : Language.values()) {
			assertTrue(r.canResolve(l));
		}
	}

	@Test
	public void nullByDefaultTest() throws IOException {
		assertEquals(null, r.resolve(new SourceFile("foo.in", "")));
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
	public void nullExample() throws IOException {
		assertEquals(null, r.resolve(new SourceFile("foo.in", "# unknown")));
	}
}