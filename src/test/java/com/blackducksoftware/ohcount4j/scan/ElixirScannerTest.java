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

public class ElixirScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.ELIXIR, new Line(Language.ELIXIR, BLANK), "\n");
        assertLine(Language.ELIXIR, new Line(Language.ELIXIR, BLANK), "     \n");
        assertLine(Language.ELIXIR, new Line(Language.ELIXIR, BLANK), "\t\n");
        assertLine(Language.ELIXIR, new Line(Language.ELIXIR, CODE), "IO.puts \"Hello World\"\n");
        assertLine(Language.ELIXIR, new Line(Language.ELIXIR, COMMENT), "# Line comment\n");
        assertLine(Language.ELIXIR, new Line(Language.ELIXIR, COMMENT), "#\n");
        assertLine(Language.ELIXIR, new Line(Language.ELIXIR, CODE), "IO.puts \"Hello World\" % with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.ELIXIR, new Line(Language.ELIXIR, BLANK), "     ");
        assertLine(Language.ELIXIR, new Line(Language.ELIXIR, BLANK), "\t");
        assertLine(Language.ELIXIR, new Line(Language.ELIXIR, CODE), "IO.puts \"Hello World\"");
        assertLine(Language.ELIXIR, new Line(Language.ELIXIR, COMMENT), "# Line comment");
        assertLine(Language.ELIXIR, new Line(Language.ELIXIR, COMMENT), "#");
        assertLine(Language.ELIXIR, new Line(Language.ELIXIR, CODE), "IO.puts \"Hello World\" # with comment");
    }

    @Test
    public void helloWorld() {
        String code = "# `if` expression\n"
                + "\n"
                + "if false do\n"
                + "  \"This will never be seen #comment after code\"\n"
                + "else\n"
                + "  \"This will\"\n"
                + "\t\n"
                + "end";

        Line[] expected = {
                new Line(Language.ELIXIR, COMMENT),
                new Line(Language.ELIXIR, BLANK),
                new Line(Language.ELIXIR, CODE),
                new Line(Language.ELIXIR, CODE),
                new Line(Language.ELIXIR, CODE),
                new Line(Language.ELIXIR, CODE),
                new Line(Language.ELIXIR, BLANK),
                new Line(Language.ELIXIR, CODE),
        };
        assertLines(Language.ELIXIR, expected, code);
    }

}
