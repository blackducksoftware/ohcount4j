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

public class ExtnPLResolverTest {

    private ExtnPLResolver r;

    @BeforeTest()
    public void setup() {
        r = new ExtnPLResolver();
    }

    @Test
    public void canResolvetest() {
        assertFalse(r.canResolve(Language.RUBY));
        assertTrue(r.canResolve(Language.PERL));
        assertTrue(r.canResolve(Language.PROLOG));
    }

    @Test
    public void perlByDefaultTest() throws IOException {
        assertEquals(Language.PERL, r.resolve(new SourceFile("foo.pl", "")));
    }

    @Test
    public void perlShebangTest() throws IOException {
        assertEquals(Language.PERL, r.resolve(new SourceFile("foo.pl", "#!/usr/bin/env perl")));
        assertEquals(Language.PERL, r.resolve(new SourceFile("foo.pl", "#!/usr/bin/perl")));
        assertEquals(Language.PERL, r.resolve(new SourceFile("foo.pl", "#!%PERL%")));
    }

    @Test
    public void prologRuleTest() throws IOException {
        assertEquals(Language.PROLOG, r.resolve(new SourceFile("foo.pl", "Foo :- Bar")));
        assertEquals(Language.PROLOG, r.resolve(new SourceFile("foo.pl", "Foo :-\n")));
    }

    @Test
    public void smileysAreNotPrologTest() throws IOException {
        assertEquals(Language.PERL, r.resolve(new SourceFile("foo.pl",
                "# This is a Perl file, even though a smiley :-) looks like Prolog\n")));
    }

    @Test
    public void perlShebangWithPrologBodyTest() throws IOException {
        // The body appears to contain Prolog, but the shebang specifies Perl,
        // so we respect the shebang and choose Perl.
        assertEquals(Language.PERL, r.resolve(new SourceFile("foo.pl",
                "#!%PERL%\n" +
                        "Foo :- Bar.\n")));
    }
}
