/*
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
