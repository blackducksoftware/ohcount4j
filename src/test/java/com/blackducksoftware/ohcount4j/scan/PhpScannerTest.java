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

public class PhpScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.PHP, new Line(Language.PHP, BLANK), "\n");
        assertLine(Language.PHP, new Line(Language.PHP, BLANK), "     \n");
        assertLine(Language.PHP, new Line(Language.PHP, BLANK), "\t\n");
        assertLine(Language.PHP, new Line(Language.PHP, CODE), "$file = fopen('file.txt', 'r+');\n");
        assertLine(Language.PHP, new Line(Language.PHP, COMMENT), "// line comment\n");
        assertLine(Language.PHP, new Line(Language.PHP, COMMENT), "//\n");
        assertLine(Language.PHP, new Line(Language.PHP, COMMENT), "# line comment\n");
        assertLine(Language.PHP, new Line(Language.PHP, COMMENT), "#\n");
        assertLine(Language.PHP, new Line(Language.PHP, CODE), "$file = fopen('file.txt', 'r+'); // with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.PHP, new Line(Language.PHP, BLANK), "     ");
        assertLine(Language.PHP, new Line(Language.PHP, BLANK), "\t");
        assertLine(Language.PHP, new Line(Language.PHP, CODE), "$file = fopen('file.txt', 'r+');");
        assertLine(Language.PHP, new Line(Language.PHP, COMMENT), "// line comment");
        assertLine(Language.PHP, new Line(Language.PHP, COMMENT), "//");
        assertLine(Language.PHP, new Line(Language.PHP, COMMENT), "# line comment");
        assertLine(Language.PHP, new Line(Language.PHP, COMMENT), "#");
        assertLine(Language.PHP, new Line(Language.PHP, CODE), "$file = fopen('file.txt', 'r+'); // with comment");
    }

    @Test
    public void sampleTest() {
        String code = "/*\n"
                + "Sample Code Written in PHP\n"
                + "		For Testing\n"
                + "*/\n"
                + "$testHereDoc = <<<HEREDOC\n"
                + "		#Anything inside this string\n"
                + "		//Is considered string\n"
                + "		/* Until the Delimiter is reached */\n"
                + "\t\n"
                + "HEREDOC;\n"
                + "function lock() {\n"
                + "		$file = fopen('file.txt', 'r+');\n"
                + "		retry:\n"
                + "		if (!flock($file, LOCK_EX & LOCK_NB)) {\n"
                + "    		goto retry;\n"
                + "		}\n"
                + "		fwrite($file, 'Success!');\n"
                + "		fclose($file);\n"
                + "		return 0;\n"
                + "}\n";

        Line[] expected = {
                new Line(Language.PHP, COMMENT),
                new Line(Language.PHP, COMMENT),
                new Line(Language.PHP, COMMENT),
                new Line(Language.PHP, COMMENT),
                new Line(Language.PHP, CODE),
                new Line(Language.PHP, CODE),
                new Line(Language.PHP, CODE),
                new Line(Language.PHP, CODE),
                new Line(Language.PHP, BLANK),
                new Line(Language.PHP, CODE),
                new Line(Language.PHP, CODE),
                new Line(Language.PHP, CODE),
                new Line(Language.PHP, CODE),
                new Line(Language.PHP, CODE),
                new Line(Language.PHP, CODE),
                new Line(Language.PHP, CODE),
                new Line(Language.PHP, CODE),
                new Line(Language.PHP, CODE),
                new Line(Language.PHP, CODE),
                new Line(Language.PHP, CODE)
        };
        assertLines(Language.PHP, expected, code);
    }

    @Test
    public void hereDoc() {
        String code = "s = <<<HERE_DOC\n"
                + "This is part of a string\n"
                + "# this is not a comment\n"
                + "\t\n"
                + "HERE_DOC\n"
                + "		//above statement is not end because no semicolon\n"
                + "HERE_DOC;\n"
                + "# this is a comment\n";

        Line[] expected = {
                new Line(Language.PHP, CODE),
                new Line(Language.PHP, CODE),
                new Line(Language.PHP, CODE),
                new Line(Language.PHP, BLANK),
                new Line(Language.PHP, CODE),
                new Line(Language.PHP, CODE),
                new Line(Language.PHP, CODE),
                new Line(Language.PHP, COMMENT)
        };
        assertLines(Language.PHP, expected, code);
    }

    @Test
    public void indentedEndHereDoc() {
        String code = "s = <<<HERE_DOC\n"
                + "This is part of a string\n"
                + "# this is not a comment\n"
                + "    HERE_DOC;\n"
                + "# this is a comment\n";

        Line[] expected = {
                new Line(Language.PHP, CODE),
                new Line(Language.PHP, CODE),
                new Line(Language.PHP, CODE),
                new Line(Language.PHP, CODE),
                new Line(Language.PHP, COMMENT)
        };
        assertLines(Language.PHP, expected, code);
    }

}
