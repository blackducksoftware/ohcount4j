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

package com.blackducksoftware.ohcount4j.scan;

import static com.blackducksoftware.ohcount4j.Entity.BLANK;
import static com.blackducksoftware.ohcount4j.Entity.CODE;
import static com.blackducksoftware.ohcount4j.Entity.COMMENT;

import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.Language;

public class CSharpScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.CSHARP, new Line(Language.CSHARP, BLANK), "\n");
        assertLine(Language.CSHARP, new Line(Language.CSHARP, BLANK), "     \n");
        assertLine(Language.CSHARP, new Line(Language.CSHARP, BLANK), "\t\n");
        assertLine(Language.CSHARP, new Line(Language.CSHARP, CODE), "using System;\n");
        assertLine(Language.CSHARP, new Line(Language.CSHARP, COMMENT), "/* Block Comment */\n");
        assertLine(Language.CSHARP, new Line(Language.CSHARP, COMMENT), "// Line comment\n");
        assertLine(Language.CSHARP, new Line(Language.CSHARP, CODE), "Console.WriteLine(\"Hello, World!\"); // Single line comment on code line\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.CSHARP, new Line(Language.CSHARP, BLANK), "     ");
        assertLine(Language.CSHARP, new Line(Language.CSHARP, BLANK), "\t");
        assertLine(Language.CSHARP, new Line(Language.CSHARP, CODE), "using System;");
        assertLine(Language.CSHARP, new Line(Language.CSHARP, COMMENT), "/* Block Comment */");
        assertLine(Language.CSHARP, new Line(Language.CSHARP, COMMENT), "// Line comment");
        assertLine(Language.CSHARP, new Line(Language.CSHARP, CODE), "Console.WriteLine(\"Hello, World!\"); // Single line comment on code line");
    }

    @Test
    public void helloWorld() {
        String code = "/*\n"
                + "	* HelloWorld\n"
                + "\t\n"
                + " * Simple C# program for testing purposes\n"
                + " */\n"
                + "using System;\n"
                + "\n"
                + "public class HelloWorld {\n"
                + "		// Single line comment\n"
                + "		public static void Main() {\n"
                + "			Console.WriteLine(\"Hello, World!\"); // Single line comment on code line\n"
                + "		}\n"
                + "}\n";

        Line[] expected = {
                new Line(Language.CSHARP, COMMENT),
                new Line(Language.CSHARP, COMMENT),
                new Line(Language.CSHARP, BLANK),
                new Line(Language.CSHARP, COMMENT),
                new Line(Language.CSHARP, COMMENT),
                new Line(Language.CSHARP, CODE),
                new Line(Language.CSHARP, BLANK),
                new Line(Language.CSHARP, CODE),
                new Line(Language.CSHARP, COMMENT),
                new Line(Language.CSHARP, CODE),
                new Line(Language.CSHARP, CODE),
                new Line(Language.CSHARP, CODE),
                new Line(Language.CSHARP, CODE)
        };
        assertLines(Language.CSHARP, expected, code);
    }

}
