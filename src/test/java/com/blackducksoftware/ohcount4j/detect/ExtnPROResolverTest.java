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

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;
import static org.testng.AssertJUnit.assertEquals;

import java.io.IOException;

import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.SourceFile;

public class ExtnPROResolverTest {

    private ExtnPROResolver r;

    @BeforeTest()
    public void setup() {
        r = new ExtnPROResolver();
    }

    @Test
    public void canResolvetest() {
        assertFalse(r.canResolve(Language.RUBY));
        assertTrue(r.canResolve(Language.MAKE));
        assertTrue(r.canResolve(Language.PVWAVE));
    }

    @Test
    public void returnsPvWaveByDefaultTest() throws IOException {
        assertEquals(Language.PVWAVE, r.resolve(new SourceFile("foo.pro", "")));
    }

    @Test
    public void qmakeExample() throws IOException {
        assertEquals(Language.MAKE, r.resolve(new SourceFile("foo.pro",
                "\tSOURCES += foo\n")));

        assertEquals(Language.MAKE, r.resolve(new SourceFile("foo.pro",
                "SOURCES+=FOO\n")));

        assertEquals(Language.MAKE, r.resolve(new SourceFile("foo.pro",
                " \t  CONFIG += foo\n")));

        assertEquals(Language.MAKE, r.resolve(new SourceFile("foo.pro",
                "CONFIG+=foo\n")));
    }

    @Test
    public void pvWaveExample() throws IOException {
        assertEquals(Language.PVWAVE, r.resolve(new SourceFile("foo.pro",
                "pro foo\n\n    print, 'hello world'\nend\n\n")));

        assertEquals(Language.PVWAVE, r.resolve(new SourceFile("foo.pro",
                "; comment")));
    }

}
