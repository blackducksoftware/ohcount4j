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

public class ExtnASPXResolverTest {

    private ExtnASPXResolver r;

    @BeforeTest()
    public void setup() {
        r = new ExtnASPXResolver();
    }

    @Test
    public void canResolvetest() {
        assertFalse(r.canResolve(Language.RUBY));
        assertTrue(r.canResolve(Language.ASPX_CSHARP));
        assertTrue(r.canResolve(Language.ASPX_VB));
    }

    @Test
    // With no other clues, the resolver should pick C# by default
    public void returnsCSharpByDefaultTest() throws IOException {
        assertEquals(Language.ASPX_CSHARP, r.resolve(new SourceFile("foo.aspx", "")));
    }

    @Test
    public void csExample() throws IOException {
        assertEquals(Language.ASPX_CSHARP, r.resolve(new SourceFile("foo.aspx",
                "<%@ Page Foo=\"Bar\" Language=\"C#\" %>\n")));

        assertEquals(Language.ASPX_CSHARP, r.resolve(new SourceFile("foo.aspx",
                "<%@Page Language=\"C#\"%>\n")));
    }

    @Test
    public void vbExample() throws IOException {
        assertEquals(Language.ASPX_VB, r.resolve(new SourceFile("foo.aspx",
                "<%@ Page Foo=\"Bar\" Language=\"VB\" %>\n")));

        assertEquals(Language.ASPX_VB, r.resolve(new SourceFile("foo.aspx",
                "<%@ page foo=\"bar\" language=\"vb\" %>\n")));

        assertEquals(Language.ASPX_VB, r.resolve(new SourceFile("foo.aspx",
                "<%@ Page Language=\"VB\" %>\n")));

        assertEquals(Language.ASPX_VB, r.resolve(new SourceFile("foo.aspx",
                "<%@ Page Foo=\"Bar\"  \n    Language=\"VB\" %>\n")));
    }

}
