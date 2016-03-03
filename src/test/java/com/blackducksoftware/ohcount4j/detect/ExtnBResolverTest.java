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

public class ExtnBResolverTest {

    private ExtnBResolver r;

    @BeforeTest()
    public void setup() {
        r = new ExtnBResolver();
    }

    @Test
    public void canResolvetest() {
        assertFalse(r.canResolve(Language.RUBY));
        assertTrue(r.canResolve(Language.CLASSIC_BASIC));
        assertTrue(r.canResolve(Language.STRUCTURED_BASIC));
        assertTrue(r.canResolve(Language.LIMBO));
    }

    @Test
    public void structuredBasicByDefaultTest() throws IOException {
        assertEquals(Language.STRUCTURED_BASIC, r.resolve(new SourceFile("foo.b", "")));
    }

    @Test
    public void classicBasicExamplesTest() throws IOException {
        assertEquals(Language.CLASSIC_BASIC, r.resolve(new SourceFile("foo.b", "10 PRINT 'HELLO WORLD'")));
        assertEquals(Language.CLASSIC_BASIC, r.resolve(new SourceFile("foo.b", "  100 REM")));
    }

    @Test
    public void structuredBasicExamplesTest() throws IOException {
        assertEquals(Language.STRUCTURED_BASIC, r.resolve(new SourceFile("foo.b", "PRINT 'HELLO WORLD'")));
        assertEquals(Language.STRUCTURED_BASIC, r.resolve(new SourceFile("foo.b", "    REM")));
    }

    @Test
    public void limboExamplesTest() throws IOException {
        assertEquals(Language.LIMBO, r.resolve(new SourceFile("foo.b", "implement Foo;")));
        assertEquals(Language.LIMBO, r.resolve(new SourceFile("foo.b", "include \"foo.m\";")));
    }
}
