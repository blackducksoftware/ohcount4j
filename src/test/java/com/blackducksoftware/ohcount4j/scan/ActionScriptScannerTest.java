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

public class ActionScriptScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.ACTIONSCRIPT, new Line(Language.ACTIONSCRIPT, BLANK), "\n");
        assertLine(Language.ACTIONSCRIPT, new Line(Language.ACTIONSCRIPT, BLANK), "     \n");
        assertLine(Language.ACTIONSCRIPT, new Line(Language.ACTIONSCRIPT, BLANK), "\t\n");
        assertLine(Language.ACTIONSCRIPT, new Line(Language.ACTIONSCRIPT, CODE), "var greeting:String = \"Hello World!\";\n");
        assertLine(Language.ACTIONSCRIPT, new Line(Language.ACTIONSCRIPT, COMMENT), "/* Block Comment */\n");
        assertLine(Language.ACTIONSCRIPT, new Line(Language.ACTIONSCRIPT, COMMENT), "// Line comment\n");
        assertLine(Language.ACTIONSCRIPT, new Line(Language.ACTIONSCRIPT, COMMENT), "//\n");
        assertLine(Language.ACTIONSCRIPT, new Line(Language.ACTIONSCRIPT, CODE), "var greeting:String = \"Hello World!\"; // with comment\n");
        assertLine(Language.ACTIONSCRIPT, new Line(Language.ACTIONSCRIPT, CODE), "<![CDATA[ string containing anything \\ /* */ ]]\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.ACTIONSCRIPT, new Line(Language.ACTIONSCRIPT, BLANK), "     ");
        assertLine(Language.ACTIONSCRIPT, new Line(Language.ACTIONSCRIPT, BLANK), "\t");
        assertLine(Language.ACTIONSCRIPT, new Line(Language.ACTIONSCRIPT, CODE), "var greeting:String = \"Hello World!\";");
        assertLine(Language.ACTIONSCRIPT, new Line(Language.ACTIONSCRIPT, COMMENT), "/* Block Comment */");
        assertLine(Language.ACTIONSCRIPT, new Line(Language.ACTIONSCRIPT, COMMENT), "// Line comment");
        assertLine(Language.ACTIONSCRIPT, new Line(Language.ACTIONSCRIPT, COMMENT), "//");
        assertLine(Language.ACTIONSCRIPT, new Line(Language.ACTIONSCRIPT, CODE), "var greeting:String = \"Hello World!\"; // with comment");
        assertLine(Language.ACTIONSCRIPT, new Line(Language.ACTIONSCRIPT, CODE), "<![CDATA[ string containing anything \\ /* */ ]]");
    }

    @Test
    public void helloWorld() {
        String code = "/* Hello World\n"
                + "		in ActionScript\n"
                + "\t\n"
                + "*/\n"
                + "package ca.flashdev.hello {\n"
                + "		public class HelloWorld {\n"
                + "			// Print's Hello World\n"
                + "			public function sayHello():String {\n"
                + "				var greeting:String = <![CDATA[ \n"
                + "										/* Hello World! */\n"
                + "										]];\n"
                + "				return greeting;\n"
                + "			}\n"
                + "		}\n"
                + "}\n";

        Line[] expected = {
                new Line(Language.ACTIONSCRIPT, COMMENT),
                new Line(Language.ACTIONSCRIPT, COMMENT),
                new Line(Language.ACTIONSCRIPT, BLANK),
                new Line(Language.ACTIONSCRIPT, COMMENT),
                new Line(Language.ACTIONSCRIPT, CODE),
                new Line(Language.ACTIONSCRIPT, CODE),
                new Line(Language.ACTIONSCRIPT, COMMENT),
                new Line(Language.ACTIONSCRIPT, CODE),
                new Line(Language.ACTIONSCRIPT, CODE),
                new Line(Language.ACTIONSCRIPT, CODE),
                new Line(Language.ACTIONSCRIPT, CODE),
                new Line(Language.ACTIONSCRIPT, CODE),
                new Line(Language.ACTIONSCRIPT, CODE),
                new Line(Language.ACTIONSCRIPT, CODE),
                new Line(Language.ACTIONSCRIPT, CODE)
        };
        assertLines(Language.ACTIONSCRIPT, expected, code);
    }

    @Test
    public void unterminatedCDataStringCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "<![CDATA[\nA\n\n";

        Line[] expected = {
                new Line(Language.ACTIONSCRIPT, CODE),
                new Line(Language.ACTIONSCRIPT, CODE),
                new Line(Language.ACTIONSCRIPT, BLANK)
        };
        assertLines(Language.ACTIONSCRIPT, expected, code);
    }

}
