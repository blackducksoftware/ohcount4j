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

public class PythonScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.PYTHON, new Line(Language.PYTHON, BLANK), "\n");
        assertLine(Language.PYTHON, new Line(Language.PYTHON, BLANK), "     \n");
        assertLine(Language.PYTHON, new Line(Language.PYTHON, BLANK), "\t\n");
        assertLine(Language.PYTHON, new Line(Language.PYTHON, CODE), "value = string.capitalize(word)\n");
        assertLine(Language.PYTHON, new Line(Language.PYTHON, COMMENT), "/* Block Comment */\n");
        assertLine(Language.PYTHON, new Line(Language.PYTHON, COMMENT), "// Line comment\n");
        assertLine(Language.PYTHON, new Line(Language.PYTHON, COMMENT), "# Line comment\n");
        assertLine(Language.PYTHON, new Line(Language.PYTHON, COMMENT), "//\n");
        assertLine(Language.PYTHON, new Line(Language.PYTHON, COMMENT), "#\n");
        assertLine(Language.PYTHON, new Line(Language.PYTHON, CODE), "value = string.capitalize(word) // with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.PYTHON, new Line(Language.PYTHON, BLANK), "     ");
        assertLine(Language.PYTHON, new Line(Language.PYTHON, BLANK), "\t");
        assertLine(Language.PYTHON, new Line(Language.PYTHON, CODE), "value = string.capitalize(word)");
        assertLine(Language.PYTHON, new Line(Language.PYTHON, COMMENT), "/* Block Comment */");
        assertLine(Language.PYTHON, new Line(Language.PYTHON, COMMENT), "// Line comment");
        assertLine(Language.PYTHON, new Line(Language.PYTHON, COMMENT), "# Line comment");
        assertLine(Language.PYTHON, new Line(Language.PYTHON, COMMENT), "//");
        assertLine(Language.PYTHON, new Line(Language.PYTHON, COMMENT), "#");
        assertLine(Language.PYTHON, new Line(Language.PYTHON, CODE), "value = string.capitalize(word) // with comment");
    }

    @Test
    public void helloWorld() {
        String code = "\"\"\"\n"
                + "Assuming this is file mymodule.py, then this string, being the\n"
                + "first statement in the file, will become the \"mymodule\" module's\n"
                + "docstring when the file is imported.\n"
                + "\"\"\"\n"
                + "\t\n"
                + "\"\"Not quite a docstring\"\"\n"
                + "# Hello World\n"
                + "// Written in Python\n"
                + "print \"Hello World!\";\n";

        Line[] expected = {
                new Line(Language.PYTHON, COMMENT),
                new Line(Language.PYTHON, COMMENT),
                new Line(Language.PYTHON, COMMENT),
                new Line(Language.PYTHON, COMMENT),
                new Line(Language.PYTHON, COMMENT),
                new Line(Language.PYTHON, BLANK),
                new Line(Language.PYTHON, CODE),
                new Line(Language.PYTHON, COMMENT),
                new Line(Language.PYTHON, COMMENT),
                new Line(Language.PYTHON, CODE)
        };
        assertLines(Language.PYTHON, expected, code);
    }

    @Test
    public void unterminatedDocStringCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "'''\n\n\n";

        Line[] expected = {
                new Line(Language.PYTHON, COMMENT),
                new Line(Language.PYTHON, BLANK),
                new Line(Language.PYTHON, BLANK)
        };
        assertLines(Language.PYTHON, expected, code);
    }

}
