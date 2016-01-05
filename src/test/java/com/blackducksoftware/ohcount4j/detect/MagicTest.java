/**
 * Copyright 2016 Black Duck Software, Inc.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
