package com.blackducksoftware.ohcount4j.detect;

import static org.testng.AssertJUnit.assertEquals;

import org.testng.annotations.Test;

/**
 * These tests test libmagic functionality; they only apply if libmagic is available
 *
 * @author mjregan
 * @author dbrown
 */
public class MagicTest {

    @Test
    public void buffer() {
        Magic magic = new Magic();

        if (magic.open()) {
            assertEquals(null, magic.error());
            magic.load();
            assertEquals(null, magic.error());

            String result = magic.buffer("#!/usr/bin/env ruby\n");
            assertEquals(null, magic.error());
            assert (result.toLowerCase().indexOf("ruby script", 0) > -1);

            magic.close();
        }
    }

    @Test
    public void file() {
        Magic magic = new Magic();

        if (magic.open()) {
            assertEquals(null, magic.error());
            magic.load();
            assertEquals(null, magic.error());

            String result = magic.file("src/main/java/com/blackducksoftware/ohcount4j/detect/Magic.java");
            assertEquals(null, magic.error());
            assert (result.toLowerCase().indexOf("text", 0) > -1);

            magic.close();
        }
    }
}
