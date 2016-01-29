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
import static com.blackducksoftware.ohcount4j.Language.COFFEESCRIPT;
import static com.blackducksoftware.ohcount4j.Language.JAVASCRIPT;

import java.io.File;

import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.SourceFile;

public class CoffeeScriptScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(COFFEESCRIPT, new Line(COFFEESCRIPT, BLANK), "\n");
        assertLine(COFFEESCRIPT, new Line(COFFEESCRIPT, BLANK), "     \n");
        assertLine(COFFEESCRIPT, new Line(COFFEESCRIPT, BLANK), "\t\n");
        assertLine(COFFEESCRIPT, new Line(COFFEESCRIPT, CODE), "alert \"I knew it!\" if elvis?\n");
        assertLine(COFFEESCRIPT, new Line(COFFEESCRIPT, COMMENT), "# comment \n");
        assertLine(COFFEESCRIPT, new Line(COFFEESCRIPT, CODE), "math = # with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(COFFEESCRIPT, new Line(COFFEESCRIPT, BLANK), "     ");
        assertLine(COFFEESCRIPT, new Line(COFFEESCRIPT, BLANK), "\t");
        assertLine(COFFEESCRIPT, new Line(COFFEESCRIPT, CODE), "alert \"I knew it!\" if elvis?");
        assertLine(COFFEESCRIPT, new Line(COFFEESCRIPT, COMMENT), "### comment ###");
        assertLine(COFFEESCRIPT, new Line(COFFEESCRIPT, CODE), "cubes = (math.cube num for num in list) # with comment");
    }

    @Test
    public void helloWorld() {
        String code = "# Objects:\n"
                + "math =\n"
                + "  root:   Math.sqrt\n"
                + "\n"
                + "  ###\n"
                + "   comment1\n"
                + "   comment 2 ###\n"
                + "\n"
                + "  square: square\n"
                + "  cube:   (x) -> x * square x\n";

        Line[] expected = {
                new Line(COFFEESCRIPT, COMMENT),
                new Line(COFFEESCRIPT, CODE),
                new Line(COFFEESCRIPT, CODE),
                new Line(COFFEESCRIPT, BLANK),
                new Line(COFFEESCRIPT, COMMENT),
                new Line(COFFEESCRIPT, COMMENT),
                new Line(COFFEESCRIPT, COMMENT),
                new Line(COFFEESCRIPT, BLANK),
                new Line(COFFEESCRIPT, CODE),
                new Line(COFFEESCRIPT, CODE)
        };
        assertLines(COFFEESCRIPT, expected, code);
    }

    @Test
    public void helloWorldWithSourceFile() throws Exception {
        SourceFile sourceFile = new SourceFile(new File(getSourceCodePath("coffeescript-1.coffee")));
        Line[] expected = {
                new Line(COFFEESCRIPT, COMMENT),
                new Line(COFFEESCRIPT, BLANK),
                new Line(COFFEESCRIPT, CODE),
                new Line(COFFEESCRIPT, BLANK),
                new Line(COFFEESCRIPT, COMMENT),
                new Line(COFFEESCRIPT, COMMENT),
                new Line(COFFEESCRIPT, COMMENT),
                new Line(COFFEESCRIPT, COMMENT),
                new Line(COFFEESCRIPT, BLANK),
                new Line(COFFEESCRIPT, CODE),
                new Line(COFFEESCRIPT, CODE),
                new Line(COFFEESCRIPT, CODE),
                new Line(COFFEESCRIPT, CODE),
                new Line(COFFEESCRIPT, BLANK),
                new Line(COFFEESCRIPT, CODE),
                new Line(COFFEESCRIPT, CODE),

                new Line(COFFEESCRIPT, BLANK),
                new Line(COFFEESCRIPT, BLANK),

                new Line(COFFEESCRIPT, COMMENT),
                new Line(COFFEESCRIPT, COMMENT),
                new Line(COFFEESCRIPT, COMMENT),
                new Line(COFFEESCRIPT, COMMENT),

                new Line(JAVASCRIPT, CODE),
                new Line(JAVASCRIPT, CODE),
                new Line(JAVASCRIPT, CODE),
                new Line(COFFEESCRIPT, BLANK),
        };
        assertLines(COFFEESCRIPT, expected, sourceFile);
    }

    @Test
    public void embeddedJavaScriptOnSeparateLine() {
        String code = "theSwitch = 0\n"
                + "`\n"
                + "  function myFunction() {}\n"
                + "`\n"
                + "var h1=10";

        Line[] expected = {
                new Line(COFFEESCRIPT, CODE),
                new Line(COFFEESCRIPT, CODE),
                new Line(JAVASCRIPT, CODE),
                new Line(COFFEESCRIPT, BLANK),
                new Line(COFFEESCRIPT, CODE)
        };
        assertLines(COFFEESCRIPT, expected, code);
    }

    @Test
    public void embeddedJavaScriptOnSameLineWithFunctionBlock() {
        String code = "var hi\n"
                + "var hi = `function(){return \"Hello\"}`\n"
                + "var test";

        Line[] expected = {
                new Line(COFFEESCRIPT, CODE),
                new Line(JAVASCRIPT, CODE),
                new Line(COFFEESCRIPT, CODE)
        };
        assertLines(COFFEESCRIPT, expected, code);
    }

    @Test
    public void emptyJavaScriptOnSameLine() {
        String code = "var hi\n"
                + "``\n"
                + "theBait   = 1000";

        Line[] expected = {
                new Line(COFFEESCRIPT, CODE),
                new Line(COFFEESCRIPT, CODE),
                new Line(COFFEESCRIPT, CODE)
        };
        assertLines(COFFEESCRIPT, expected, code);
    }

    @Test
    public void commentJavaScriptOnSameLine() {
        String code = "theBait   = 1000\n"
                + "`/* No code just comment */`\n"
                + "theSwitch = 0";

        Line[] expected = {
                new Line(COFFEESCRIPT, CODE),
                new Line(JAVASCRIPT, COMMENT),
                new Line(COFFEESCRIPT, CODE)
        };
        assertLines(COFFEESCRIPT, expected, code);
    }

    @Test
    public void embeddedJavaScriptOnSeparateLineWithBlock() {
        String code = "theBait   = 1000\n"
                + "`{\n"
                + "  document.write(\"Hello, world!\\n\");\n"
                + "}`\n"
                + "theSwitch = 0";

        Line[] expected = {
                new Line(COFFEESCRIPT, CODE),
                new Line(JAVASCRIPT, CODE),
                new Line(JAVASCRIPT, CODE),
                new Line(JAVASCRIPT, CODE),
                new Line(COFFEESCRIPT, CODE)
        };
        assertLines(COFFEESCRIPT, expected, code);
    }

    @Test
    public void embeddedJavaScriptOnSameLine() {
        String code = "theBait   = 1000\n"
                + "`document.write(\"Hello, world!\\n\")`\n"
                + "theSwitch = 0";

        Line[] expected = {
                new Line(COFFEESCRIPT, CODE),
                new Line(JAVASCRIPT, CODE),
                new Line(COFFEESCRIPT, CODE)
        };
        assertLines(COFFEESCRIPT, expected, code);
    }
}
