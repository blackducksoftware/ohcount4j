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
import static com.blackducksoftware.ohcount4j.Language.EC;

import org.testng.annotations.Test;

public class ECScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(EC, new Line(EC, BLANK), "\n");
        assertLine(EC, new Line(EC, BLANK), "     \n");
        assertLine(EC, new Line(EC, BLANK), "\t\n");
        assertLine(EC, new Line(EC, CODE), "import \"ecere\"\n");
        assertLine(EC, new Line(EC, COMMENT), "/* driver = \"OpenGL\"; */\n");
        assertLine(EC, new Line(EC, COMMENT), "// driver = \"OpenGL\";\n");
        assertLine(EC, new Line(EC, COMMENT), "//\n");
        assertLine(EC, new Line(EC, CODE), "ModelViewer modelViewer { }; // with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(EC, new Line(EC, BLANK), "     ");
        assertLine(EC, new Line(EC, BLANK), "\t");
        assertLine(EC, new Line(EC, CODE), "class eModelApp : GuiApplication");
        assertLine(EC, new Line(EC, COMMENT), "/* Block Comment */");
        assertLine(EC, new Line(EC, COMMENT), "// Line comment");
        assertLine(EC, new Line(EC, COMMENT), "//");
        assertLine(EC, new Line(EC, CODE), "class eModelApp : GuiApplication // with comment");
    }

    @Test
    public void helloWorld() {
        String code = "FileDialog fileDialog\n"
                + "{\n"
                + "\t\n"
                + " \t\n"
                + "\n"
                + " // driver = \"OpenGL\";\n"
                + "   filters = fileFilters, sizeFilters = sizeof(fileFilters);\n"
                + "   text = \"Select a 3DS model to display...\"\n"
                + "};\n";

        Line[] expected = {
                new Line(EC, CODE),
                new Line(EC, CODE),
                new Line(EC, BLANK),
                new Line(EC, BLANK),
                new Line(EC, BLANK),
                new Line(EC, COMMENT),
                new Line(EC, CODE),
                new Line(EC, CODE),
                new Line(EC, CODE)
        };
        assertLines(EC, expected, code);
    }

    @Test
    public void unterminatedMultilineStringCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "'\nA\n\n";

        Line[] expected = {
                new Line(EC, CODE),
                new Line(EC, CODE),
                new Line(EC, BLANK)
        };
        assertLines(EC, expected, code);
    }

    @Test
    public void unterminatedBlockCommentCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "/*\n\n\n";

        Line[] expected = {
                new Line(EC, COMMENT),
                new Line(EC, BLANK),
                new Line(EC, BLANK)
        };
        assertLines(EC, expected, code);
    }
}
