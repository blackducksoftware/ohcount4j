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

public class DScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.D, new Line(Language.D, BLANK), "\n");
        assertLine(Language.D, new Line(Language.D, BLANK), "     \n");
        assertLine(Language.D, new Line(Language.D, BLANK), "\t\n");
        assertLine(Language.D, new Line(Language.D, CODE), "pragma(msg, Format!(\"7! = %s\", fact_7));\n");
        assertLine(Language.D, new Line(Language.D, COMMENT), "// Line comment\n");
        assertLine(Language.D, new Line(Language.D, COMMENT), "/+ Block comment +/\n");
        assertLine(Language.D, new Line(Language.D, COMMENT), "/* Block comment */\n");
        assertLine(Language.D, new Line(Language.D, COMMENT), "//\n");
        assertLine(Language.D, new Line(Language.D, CODE), "mixin(fooToD(import(\"example.foo\"))); // with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.D, new Line(Language.D, BLANK), "     ");
        assertLine(Language.D, new Line(Language.D, BLANK), "\t");
        assertLine(Language.D, new Line(Language.D, CODE), "pragma(msg, Format!(\"7! = %s\", fact_7));");
        assertLine(Language.D, new Line(Language.D, COMMENT), "// Line comment");
        assertLine(Language.D, new Line(Language.D, COMMENT), "/+ Block comment +/");
        assertLine(Language.D, new Line(Language.D, COMMENT), "/* Block comment */");
        assertLine(Language.D, new Line(Language.D, COMMENT), "//");
        assertLine(Language.D, new Line(Language.D, CODE), "mixin(fooToD(import(\"example.foo\"))); // with comment");
    }

    @Test
    public void sampleTest() {
        String code = "/+ Simple D Program\n"
                + "			/+ For Testing Purposes\n"
                + "\t\n"
                + "			+/\n"
                + "+/\n"
                + "void foo()\n"
                + "{\n"
                + "		bool cont = true;\n"
                + "		while (cont)\n"
                + "		{\n"
                + "			receive( // delegates are used to match the message type\n"
                + "				(int msg) => writeln(\"int received: \", msg),\n"
                + "				(Tid sender) { cont = false; sender.send(-1); },\n"
                + "				(Variant v) => writeln(\"huh?\") // Variant matches any type\n"
                + "			);\n"
                + "		}\n"
                + "}\n";

        Line[] expected = {
                new Line(Language.D, COMMENT),
                new Line(Language.D, COMMENT),
                new Line(Language.D, BLANK),
                new Line(Language.D, COMMENT),
                new Line(Language.D, COMMENT),
                new Line(Language.D, CODE),
                new Line(Language.D, CODE),
                new Line(Language.D, CODE),
                new Line(Language.D, CODE),
                new Line(Language.D, CODE),
                new Line(Language.D, CODE),
                new Line(Language.D, CODE),
                new Line(Language.D, CODE),
                new Line(Language.D, CODE),
                new Line(Language.D, CODE),
                new Line(Language.D, CODE),
                new Line(Language.D, CODE)
        };
        assertLines(Language.D, expected, code);
    }

    @Test
    public void unterminatedNestedBlockCommentCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "/+\n /+\n +/\n\n\n";

        Line[] expected = {
                new Line(Language.D, COMMENT),
                new Line(Language.D, COMMENT),
                new Line(Language.D, COMMENT),
                new Line(Language.D, BLANK),
                new Line(Language.D, BLANK)
        };
        assertLines(Language.D, expected, code);
    }
}
