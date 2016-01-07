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

/**
 * @author gandhip
 *
 */
public class ExtnCSResolverTest {

    private ExtnCSResolver resolver;

    @BeforeTest()
    public void setup() {
        resolver = new ExtnCSResolver();
    }

    @Test
    public void canResolvetest() {
        assertFalse(resolver.canResolve(Language.RUBY));
        assertFalse(resolver.canResolve(Language.C));

        assertTrue(resolver.canResolve(Language.CSHARP));
        assertTrue(resolver.canResolve(Language.CLEARSILVER));
    }

    @Test
    public void returnsCSharpByDefaultTest() throws IOException {
        // With no other clues, the resolver should pick CSHARP by default
        assertEquals(Language.CSHARP, resolver.resolve(new SourceFile("main.cs", "")));
    }

    @Test
    public void cSharpExample() throws IOException {
        SourceFile s = new SourceFile("main.cs",
                "public class SomeClass{\n"
                        + "public static void Main () {\n"
                        + "SomeClass x = new SomeClass ();\n"
                        + "}\n"
                        + "}"
                );
        assertEquals(Language.CSHARP, resolver.resolve(s));
    }

    @Test
    public void clearSilverExample() throws IOException {
        SourceFile s = new SourceFile("main.cs",
                "<?cs include:\"templates/header.cs\" ?>\n"
                        + "<?cs if:?error ?>\n"
                        + "<div class=\"error\"><?cs var:error ?></div>\n"
                        + "<?cs /if ?>");
        assertEquals(Language.CLEARSILVER, resolver.resolve(s));
    }
}
