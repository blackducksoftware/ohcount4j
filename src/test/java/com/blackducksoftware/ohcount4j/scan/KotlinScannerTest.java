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
import static com.blackducksoftware.ohcount4j.Language.KOTLIN;

import org.testng.annotations.Test;


public class KotlinScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(KOTLIN, new Line(KOTLIN, BLANK), "\n");
        assertLine(KOTLIN, new Line(KOTLIN, BLANK), "     \n");
        assertLine(KOTLIN, new Line(KOTLIN, BLANK), "\t\n");
        assertLine(KOTLIN, new Line(KOTLIN, CODE), "package hello\n");
        assertLine(KOTLIN, new Line(KOTLIN, COMMENT), "/* Block Comment */\n");
        assertLine(KOTLIN, new Line(KOTLIN, COMMENT), "/** Block Comment **/\n");
        assertLine(KOTLIN, new Line(KOTLIN, COMMENT), "// Line comment\n");
        assertLine(KOTLIN, new Line(KOTLIN, COMMENT), "//\n");
        assertLine(KOTLIN, new Line(KOTLIN, CODE), "fun main(args: Array<String>) { // with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(KOTLIN, new Line(KOTLIN, BLANK), "     ");
        assertLine(KOTLIN, new Line(KOTLIN, BLANK), "\t");
        assertLine(KOTLIN, new Line(KOTLIN, CODE), "package hello");
        assertLine(KOTLIN, new Line(KOTLIN, COMMENT), "/* Block Comment */");
        assertLine(KOTLIN, new Line(KOTLIN, COMMENT), "/** Block Comment **/");
        assertLine(KOTLIN, new Line(KOTLIN, COMMENT), "// Line comment");
        assertLine(KOTLIN, new Line(KOTLIN, COMMENT), "//");
        assertLine(KOTLIN, new Line(KOTLIN, CODE), "fun add(member: T): Int { } // with comment");
    }

    @Test
    public void helloWorld() {
    	String code = "/* Hello World in Kotlin\n"
    			 + "* with multi-line comment */\n"
    			 + "\n"
    			 + "// Hello World in Kotlin\n"
    			 + "\n"
    			 + "class Greeter(val name: String) {\n"
    			 + "fun greet() { \n"
    			 + "     println(\"Hello, $name\")\n"
    			 + "  }\n"
    			 + "}\n"
    			 + "	\n"
    			 + "fun main(args: Array<String>) {\n"
    			 + "Greeter(args[0]).greet()\n"
    			 + "/* Hello World in Kotlin\n"
    			 + " * with multi-line comment */\n"
    			 + "    			// Hello World in Kotlin\n"
    			 + "}  \n";

        Line[] expected = {
                new Line(KOTLIN, COMMENT),
                new Line(KOTLIN, COMMENT),
                new Line(KOTLIN, BLANK),
                new Line(KOTLIN, COMMENT),
                new Line(KOTLIN, BLANK),
                new Line(KOTLIN, CODE),
                new Line(KOTLIN, CODE),
                new Line(KOTLIN, CODE),
                new Line(KOTLIN, CODE),
                new Line(KOTLIN, CODE),
                new Line(KOTLIN, BLANK),
                new Line(KOTLIN, CODE),
                new Line(KOTLIN, CODE),
                new Line(KOTLIN, COMMENT),
                new Line(KOTLIN, COMMENT),
                new Line(KOTLIN, COMMENT),
                new Line(KOTLIN, CODE)
        };
        assertLines(KOTLIN, expected, code);
    }

    @Test
    public void unterminatedMultilineStringCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "'\nA\n\n";

        Line[] expected = {
                new Line(KOTLIN, CODE),
                new Line(KOTLIN, CODE),
                new Line(KOTLIN, BLANK)
        };
        assertLines(KOTLIN, expected, code);
    }

    @Test
    public void unterminatedBlockCommentCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "/*\n\n\n";

        Line[] expected = {
                new Line(KOTLIN, COMMENT),
                new Line(KOTLIN, BLANK),
                new Line(KOTLIN, BLANK)
        };
        assertLines(KOTLIN, expected, code);
    }
    
}
