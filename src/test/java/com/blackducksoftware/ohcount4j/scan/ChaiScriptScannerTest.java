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

package com.blackducksoftware.ohcount4j.scan;

import static com.blackducksoftware.ohcount4j.Entity.BLANK;
import static com.blackducksoftware.ohcount4j.Entity.CODE;
import static com.blackducksoftware.ohcount4j.Entity.COMMENT;

import java.io.File;

import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.SourceFile;

/**
 * @author gandhip
 *
 */
public class ChaiScriptScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.CHAISCRIPT, new Line(Language.CHAISCRIPT, BLANK), "\n");
        assertLine(Language.CHAISCRIPT, new Line(Language.CHAISCRIPT, BLANK), "     \n");
        assertLine(Language.CHAISCRIPT, new Line(Language.CHAISCRIPT, BLANK), "\t\n");
        assertLine(Language.CHAISCRIPT, new Line(Language.CHAISCRIPT, CODE), "load_module(\"stl_extra\")\n");
        assertLine(Language.CHAISCRIPT, new Line(Language.CHAISCRIPT, COMMENT), "/* Block Comment */\n");
        assertLine(Language.CHAISCRIPT, new Line(Language.CHAISCRIPT, COMMENT), "// Line comment\n");
        assertLine(Language.CHAISCRIPT, new Line(Language.CHAISCRIPT, COMMENT), "//\n");
        assertLine(Language.CHAISCRIPT, new Line(Language.CHAISCRIPT, CODE), "var v = [] // with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.CHAISCRIPT, new Line(Language.CHAISCRIPT, BLANK), "     ");
        assertLine(Language.CHAISCRIPT, new Line(Language.CHAISCRIPT, BLANK), "\t");
        assertLine(Language.CHAISCRIPT, new Line(Language.CHAISCRIPT, CODE), "load_module(\"stl_extra\")");
        assertLine(Language.CHAISCRIPT, new Line(Language.CHAISCRIPT, COMMENT), "/* Block Comment */");
        assertLine(Language.CHAISCRIPT, new Line(Language.CHAISCRIPT, COMMENT), "// Line comment");
        assertLine(Language.CHAISCRIPT, new Line(Language.CHAISCRIPT, COMMENT), "//");
        assertLine(Language.CHAISCRIPT, new Line(Language.CHAISCRIPT, CODE), "var v = [] // with comment");
    }

    @Test
    public void helloWorld() {
        String code = "/* Hello World\n"
                + " * with multi-line comment */\n"
                + "\n"
                + "var submenu=1;\n"
                + "\n"
                + "def print() {\n"
                + "  print(\"Hello world!\")\n"
                + "}";

        Line[] expected = {
                new Line(Language.CHAISCRIPT, COMMENT),
                new Line(Language.CHAISCRIPT, COMMENT),
                new Line(Language.CHAISCRIPT, BLANK),
                new Line(Language.CHAISCRIPT, CODE),
                new Line(Language.CHAISCRIPT, BLANK),
                new Line(Language.CHAISCRIPT, CODE),
                new Line(Language.CHAISCRIPT, CODE),
                new Line(Language.CHAISCRIPT, CODE)
        };
        assertLines(Language.CHAISCRIPT, expected, code);
    }

    @Test
    public void sampleTestWithSourceFile() throws Exception {
        SourceFile sourceFile = new SourceFile(new File(getSourceCodePath("chaiscript.chai")));
        Line[] expected = {
                new Line(Language.CHAISCRIPT, COMMENT),
                new Line(Language.CHAISCRIPT, COMMENT),
                new Line(Language.CHAISCRIPT, COMMENT),
                new Line(Language.CHAISCRIPT, COMMENT),
                new Line(Language.CHAISCRIPT, CODE),
                new Line(Language.CHAISCRIPT, COMMENT),
                new Line(Language.CHAISCRIPT, COMMENT),
                new Line(Language.CHAISCRIPT, CODE),
                new Line(Language.CHAISCRIPT, CODE),
                new Line(Language.CHAISCRIPT, CODE),
                new Line(Language.CHAISCRIPT, CODE),
                new Line(Language.CHAISCRIPT, CODE),
                new Line(Language.CHAISCRIPT, BLANK),
                new Line(Language.CHAISCRIPT, BLANK),
                new Line(Language.CHAISCRIPT, COMMENT),
                new Line(Language.CHAISCRIPT, CODE),
                new Line(Language.CHAISCRIPT, CODE)
        };
        assertLines(Language.CHAISCRIPT, expected, sourceFile);
    }

}
