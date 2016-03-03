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

public class GroovyScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.GROOVY, new Line(Language.GROOVY, BLANK), "\n");
        assertLine(Language.GROOVY, new Line(Language.GROOVY, BLANK), "     \n");
        assertLine(Language.GROOVY, new Line(Language.GROOVY, BLANK), "\t\n");
        assertLine(Language.GROOVY, new Line(Language.GROOVY, CODE), "def name='World'; println \"Hello $name!\"\n");
        assertLine(Language.GROOVY, new Line(Language.GROOVY, COMMENT), "/* Block Comment */\n");
        assertLine(Language.GROOVY, new Line(Language.GROOVY, COMMENT), "// Line comment\n");
        assertLine(Language.GROOVY, new Line(Language.GROOVY, COMMENT), "//\n");
        assertLine(Language.GROOVY, new Line(Language.GROOVY, CODE), "def name='World'; println \"Hello $name!\" // with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.GROOVY, new Line(Language.GROOVY, BLANK), "     ");
        assertLine(Language.GROOVY, new Line(Language.GROOVY, BLANK), "\t");
        assertLine(Language.GROOVY, new Line(Language.GROOVY, CODE), "def name='World'; println \"Hello $name!\"");
        assertLine(Language.GROOVY, new Line(Language.GROOVY, COMMENT), "/* Block Comment */");
        assertLine(Language.GROOVY, new Line(Language.GROOVY, COMMENT), "// Line comment");
        assertLine(Language.GROOVY, new Line(Language.GROOVY, COMMENT), "//");
        assertLine(Language.GROOVY, new Line(Language.GROOVY, CODE), "def name='World'; println \"Hello $name!\" // with comment");
    }

    @Test
    public void helloWorld() {
        String code = "/* Groovy Style\n"
                + "\n"
                + "Hello World Program*/\n"
                + "class Greet {\n"
                + "	  def name\n"
                + "	  Greet(who) { name = who[0].toUpperCase() +\n"
                + "	                      who[1..-1] }\n"
                + "	  def salute() { println \"Hello $name!\" }\n"
                + "}\n"
                + "\n"
                + "g = new Greet('world')  // create object\n"
                + "g.salute()              // output \"Hello World!\";\n";

        Line[] expected = {
                new Line(Language.GROOVY, COMMENT),
                new Line(Language.GROOVY, BLANK),
                new Line(Language.GROOVY, COMMENT),
                new Line(Language.GROOVY, CODE),
                new Line(Language.GROOVY, CODE),
                new Line(Language.GROOVY, CODE),
                new Line(Language.GROOVY, CODE),
                new Line(Language.GROOVY, CODE),
                new Line(Language.GROOVY, CODE),
                new Line(Language.GROOVY, BLANK),
                new Line(Language.GROOVY, CODE),
                new Line(Language.GROOVY, CODE)
        };
        assertLines(Language.GROOVY, expected, code);
    }

}
