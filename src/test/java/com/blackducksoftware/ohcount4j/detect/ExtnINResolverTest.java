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
