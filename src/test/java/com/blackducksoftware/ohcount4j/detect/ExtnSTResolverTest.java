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

public class ExtnSTResolverTest {

    private ExtnSTResolver r;

    @BeforeTest()
    public void setup() {
        r = new ExtnSTResolver();
    }

    @Test
    public void canResolvetest() {
        assertFalse(r.canResolve(Language.RUBY));
        assertTrue(r.canResolve(Language.SMALLTALK));
    }

    @Test
    public void nullByDefaultTest() throws IOException {
        assertEquals(null, r.resolve(new SourceFile("foo.st", "")));
    }

    @Test
    public void smalltalkExamplesTest() throws IOException {
        assertEquals(Language.SMALLTALK, r.resolve(new SourceFile("foo.st",
                "result := a > b\n" +
                        "\tifTrue:[ 'greater' ]\n" +
                        "\tifFalse:[ 'less or equal' ]\n")));
        assertEquals(Language.SMALLTALK, r.resolve(new SourceFile("foo.st",
                "| aString vowels |\n" +
                        "aString := 'This is a string'.\n" +
                        "vowels := aString select: [:aCharacter | aCharacter isVowel].\n")));
    }

    @Test
    public void notSmalltalkExamplesTest() throws IOException {
        assertEquals(null, r.resolve(new SourceFile("foo.st", "foo")));
        assertEquals(null, r.resolve(new SourceFile("foo.st", "")));
    }
}
