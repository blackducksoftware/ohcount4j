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

import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.Language;

/**
 * @author gandhip
 *
 */
public class GoLangScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.GOLANG, new Line(Language.GOLANG, BLANK), "\n");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, BLANK), "     \n");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, BLANK), "\t\n");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, CODE), "package main;\n");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, CODE), "import \"fmt\";\n");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, COMMENT), "/* Block Comment */\n");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, COMMENT), "// Line comment\n");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, COMMENT), "//\n");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, CODE), "import \"fmt\"; // with comment\n");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, CODE), "func functionDeclaration() {");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, CODE), "fmt.Println(\"Hello, World!\")");
    }

    @Test
    public void helloWorld() {
        String code = "/* Hello World\n"
                + " * with multi-line comment */\n"
                + "\n"
                + "package main\n"
                + "\n"
                + "import \"fmt\"\n"
                + "\n"
                + "\tfunc main() { \n"
                + "// single line comment\n"
                + "\t\tvar x float64 = 20.0\n"
                + "\t\tfmt.Println(\"Hello, World!\")\n"
                + "\t}\n";
        Line[] expected = {
                new Line(Language.GOLANG, COMMENT),
                new Line(Language.GOLANG, COMMENT),
                new Line(Language.GOLANG, BLANK),
                new Line(Language.GOLANG, CODE),
                new Line(Language.GOLANG, BLANK),
                new Line(Language.GOLANG, CODE),
                new Line(Language.GOLANG, BLANK),
                new Line(Language.GOLANG, CODE),
                new Line(Language.GOLANG, COMMENT),
                new Line(Language.GOLANG, CODE),
                new Line(Language.GOLANG, CODE),
                new Line(Language.GOLANG, CODE)
        };
        assertLines(Language.GOLANG, expected, code);
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.GOLANG, new Line(Language.GOLANG, BLANK), "     ");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, BLANK), "\t");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, CODE), "package main;");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, CODE), "import \"fmt\";");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, COMMENT), "/* Block Comment */");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, COMMENT), "// Line comment");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, COMMENT), "//");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, CODE), "import \"fmt\"; // with comment");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, CODE), "func functionDeclaration() {");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, CODE), "fmt.Println(\"Hello, World!\")");
    }

    @Test
    public void unterminatedMultilineStringCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "'\nA\n\n";

        Line[] expected = {
                new Line(Language.GOLANG, CODE),
                new Line(Language.GOLANG, CODE),
                new Line(Language.GOLANG, BLANK)
        };
        assertLines(Language.GOLANG, expected, code);
    }

    @Test
    public void unterminatedBlockCommentCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "/*\n\n\n";

        Line[] expected = {
                new Line(Language.GOLANG, COMMENT),
                new Line(Language.GOLANG, BLANK),
                new Line(Language.GOLANG, BLANK)
        };
        assertLines(Language.GOLANG, expected, code);
    }
}
