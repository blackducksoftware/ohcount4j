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

public class JavaScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.JAVA, new Line(Language.JAVA, BLANK), "\n");
        assertLine(Language.JAVA, new Line(Language.JAVA, BLANK), "     \n");
        assertLine(Language.JAVA, new Line(Language.JAVA, BLANK), "\t\n");
        assertLine(Language.JAVA, new Line(Language.JAVA, CODE), "import java.util.List;\n");
        assertLine(Language.JAVA, new Line(Language.JAVA, COMMENT), "/* Block Comment */\n");
        assertLine(Language.JAVA, new Line(Language.JAVA, COMMENT), "/** Block Comment **/\n");
        assertLine(Language.JAVA, new Line(Language.JAVA, COMMENT), "// Line comment\n");
        assertLine(Language.JAVA, new Line(Language.JAVA, COMMENT), "//\n");
        assertLine(Language.JAVA, new Line(Language.JAVA, CODE), "import java.util.List; // with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.JAVA, new Line(Language.JAVA, BLANK), "     ");
        assertLine(Language.JAVA, new Line(Language.JAVA, BLANK), "\t");
        assertLine(Language.JAVA, new Line(Language.JAVA, CODE), "import java.util.List;");
        assertLine(Language.JAVA, new Line(Language.JAVA, COMMENT), "/* Block Comment */");
        assertLine(Language.JAVA, new Line(Language.JAVA, COMMENT), "/** Block Comment **/");
        assertLine(Language.JAVA, new Line(Language.JAVA, COMMENT), "// Line comment");
        assertLine(Language.JAVA, new Line(Language.JAVA, COMMENT), "//");
        assertLine(Language.JAVA, new Line(Language.JAVA, CODE), "import java.util.List; // with comment");
    }

    @Test
    public void helloWorld() {
        String code = "/* Hello World\n"
                + " * with multi-line comment */\n"
                + "\n"
                + "class HelloWorldApp {\n"
                + "\tpublic static void main(String[] args) {\n"
                + "\t\tSystem.out.println(\"Hello world!\");\n"
                + "/** comment with 2 *s **/\n"
                + "\t}\n"
                + "}";

        Line[] expected = {
                new Line(Language.JAVA, COMMENT),
                new Line(Language.JAVA, COMMENT),
                new Line(Language.JAVA, BLANK),
                new Line(Language.JAVA, CODE),
                new Line(Language.JAVA, CODE),
                new Line(Language.JAVA, CODE),
                new Line(Language.JAVA, COMMENT),
                new Line(Language.JAVA, CODE),
                new Line(Language.JAVA, CODE)
        };
        assertLines(Language.JAVA, expected, code);
    }

    @Test
    public void unterminatedMultilineStringCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "'\nA\n\n";

        Line[] expected = {
                new Line(Language.JAVA, CODE),
                new Line(Language.JAVA, CODE),
                new Line(Language.JAVA, BLANK)
        };
        assertLines(Language.JAVA, expected, code);
    }

    @Test
    public void unterminatedBlockCommentCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "/*\n\n\n";

        Line[] expected = {
                new Line(Language.JAVA, COMMENT),
                new Line(Language.JAVA, BLANK),
                new Line(Language.JAVA, BLANK)
        };
        assertLines(Language.JAVA, expected, code);
    }
}
