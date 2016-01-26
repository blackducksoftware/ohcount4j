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
import static com.blackducksoftware.ohcount4j.Language.SCALA;

import org.testng.annotations.Test;

public class ScalaScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(SCALA, new Line(SCALA, BLANK), "\n");
        assertLine(SCALA, new Line(SCALA, BLANK), "     \n");
        assertLine(SCALA, new Line(SCALA, BLANK), "\t\n");
        assertLine(SCALA, new Line(SCALA, CODE), "import java.io._\n");
        assertLine(SCALA, new Line(SCALA, COMMENT), "/* Block Comment */\n");
        assertLine(SCALA, new Line(SCALA, COMMENT), "/** Block Comment **/\n");
        assertLine(SCALA, new Line(SCALA, COMMENT), "/*** Block Comment ***/\n");
        assertLine(SCALA, new Line(SCALA, COMMENT), "// Line comment\n");
        assertLine(SCALA, new Line(SCALA, COMMENT), "//\n");
        assertLine(SCALA, new Line(SCALA, CODE), "import java.io._ // with comment\n");
        assertLine(SCALA, new Line(SCALA, CODE), "val counts = holmes.split(\"\\s+\").groupBy(x=>x)");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(SCALA, new Line(SCALA, BLANK), "     ");
        assertLine(SCALA, new Line(SCALA, BLANK), "\t");
        assertLine(SCALA, new Line(SCALA, CODE), "import java.io._");
        assertLine(SCALA, new Line(SCALA, COMMENT), "/* Block Comment */");
        assertLine(SCALA, new Line(SCALA, COMMENT), "/** Block Comment **/");
        assertLine(SCALA, new Line(SCALA, COMMENT), "/*** Block Comment ***/");
        assertLine(SCALA, new Line(SCALA, COMMENT), "// Line comment");
        assertLine(SCALA, new Line(SCALA, COMMENT), "//");
        assertLine(SCALA, new Line(SCALA, CODE), "import java.io._ // with comment");
    }

    @Test
    public void helloWorld() {
        String code = "/* Hello World\n"
                + " * with multi-line comment */\n"
                + "\n"
                + "import java.io._\n"
                + "\n"
                + "object HelloWorld {\n"
                + "    def main(args: Array[String]): Unit = {\n"
                + "    println(\"Hello, world!\")\n"
                + "/** comment yet again **/\n"
                + "/*** comment yet again ***/\n"
                + "}\n}";

        Line[] expected = {
                new Line(SCALA, COMMENT),
                new Line(SCALA, COMMENT),
                new Line(SCALA, BLANK),
                new Line(SCALA, CODE),
                new Line(SCALA, BLANK),
                new Line(SCALA, CODE),
                new Line(SCALA, CODE),
                new Line(SCALA, CODE),
                new Line(SCALA, COMMENT),
                new Line(SCALA, COMMENT),
                new Line(SCALA, CODE),
                new Line(SCALA, CODE)
        };
        assertLines(SCALA, expected, code);
    }

    @Test
    public void unterminatedMultilineStringCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "'\nA\n\n";

        Line[] expected = {
                new Line(SCALA, CODE),
                new Line(SCALA, CODE),
                new Line(SCALA, BLANK)
        };
        assertLines(SCALA, expected, code);
    }

    @Test
    public void unterminatedBlockCommentCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "/*\n\n\n";

        Line[] expected = {
                new Line(SCALA, COMMENT),
                new Line(SCALA, BLANK),
                new Line(SCALA, BLANK)
        };
        assertLines(SCALA, expected, code);
    }

}
