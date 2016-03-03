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

public class RubyScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.RUBY, new Line(Language.RUBY, BLANK), "\n");
        assertLine(Language.RUBY, new Line(Language.RUBY, BLANK), "     \n");
        assertLine(Language.RUBY, new Line(Language.RUBY, BLANK), "\t\n");
        assertLine(Language.RUBY, new Line(Language.RUBY, CODE), "require 'lib'\n");
        assertLine(Language.RUBY, new Line(Language.RUBY, COMMENT), "# line comment\n");
        assertLine(Language.RUBY, new Line(Language.RUBY, COMMENT), "#\n");
        assertLine(Language.RUBY, new Line(Language.RUBY, CODE), "require 'lib' // with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.RUBY, new Line(Language.RUBY, BLANK), "     ");
        assertLine(Language.RUBY, new Line(Language.RUBY, BLANK), "\t");
        assertLine(Language.RUBY, new Line(Language.RUBY, CODE), "require 'lib'");
        assertLine(Language.RUBY, new Line(Language.RUBY, COMMENT), "# line comment");
        assertLine(Language.RUBY, new Line(Language.RUBY, COMMENT), "#");
        assertLine(Language.RUBY, new Line(Language.RUBY, CODE), "require 'lib' // with comment");
    }

    @Test
    public void helloWorld() {
        String code = "# Hello World\n"
                + "\n"
                + "puts 'Hello world!'";

        Line[] expected = {
                new Line(Language.RUBY, COMMENT),
                new Line(Language.RUBY, BLANK),
                new Line(Language.RUBY, CODE)
        };
        assertLines(Language.RUBY, expected, code);
    }

    @Test
    public void blockComment() {
        String code = "some_code()\n"
                + "=begin\n"
                + "This is part of a block comment\n"
                + "\n"
                + "A blank line is included above\n"
                + "=end\n"
                + "more_code()\n";

        Line[] expected = {
                new Line(Language.RUBY, CODE),
                new Line(Language.RUBY, CODE),
                new Line(Language.RUBY, COMMENT),
                new Line(Language.RUBY, BLANK),
                new Line(Language.RUBY, COMMENT),
                new Line(Language.RUBY, CODE),
                new Line(Language.RUBY, CODE)
        };
        assertLines(Language.RUBY, expected, code);
    }

    @Test
    public void hereDoc() {
        String code = "s = <<HERE_DOC\n"
                + "This is part of a string\n"
                + "# this is not a comment\n"
                + "HERE_DOC\n"
                + "# this is a comment\n";

        Line[] expected = {
                new Line(Language.RUBY, CODE),
                new Line(Language.RUBY, CODE),
                new Line(Language.RUBY, CODE),
                new Line(Language.RUBY, CODE),
                new Line(Language.RUBY, COMMENT)
        };
        assertLines(Language.RUBY, expected, code);
    }

    @Test
    public void indentedHereDoc() {
        String code = "s = <<-HERE_DOC\n"
                + "This is part of a string\n"
                + "# this is not a comment\n"
                + "    HERE_DOC\n"
                + "# this is a comment\n";

        Line[] expected = {
                new Line(Language.RUBY, CODE),
                new Line(Language.RUBY, CODE),
                new Line(Language.RUBY, CODE),
                new Line(Language.RUBY, CODE),
                new Line(Language.RUBY, COMMENT)
        };
        assertLines(Language.RUBY, expected, code);
    }

}
