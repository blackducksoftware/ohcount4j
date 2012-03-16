package net.ohloh.ohcount4j.detect;

import org.testng.annotations.Test;
import static org.testng.AssertJUnit.assertEquals;

public class MagicTest {

	@Test
	public void buffer() {
		Magic magic = new Magic();
		magic.open();
		assertEquals(null, magic.error());
		magic.load();
		assertEquals(null, magic.error());

		String result = magic.buffer("#!/usr/bin/env ruby\n");
		assertEquals(null, magic.error());
		assert(result.indexOf("ruby script", 0) > -1);

		magic.close();
	}

	@Test
	public void file() {
		Magic magic = new Magic();
		magic.open();
		assertEquals(null, magic.error());
		magic.load();
		assertEquals(null, magic.error());

		String result = magic.file("src/net/ohloh/ohcount4j/detect/Magic.java");
		assertEquals(null, magic.error());
		assert(result.indexOf("Java program", 0) > -1);

		magic.close();
	}
}