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

public class ErlangScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.ERLANG, new Line(Language.ERLANG, BLANK), "\n");
        assertLine(Language.ERLANG, new Line(Language.ERLANG, BLANK), "     \n");
        assertLine(Language.ERLANG, new Line(Language.ERLANG, BLANK), "\t\n");
        assertLine(Language.ERLANG, new Line(Language.ERLANG, CODE), "import Erlang.util.List;\n");
        assertLine(Language.ERLANG, new Line(Language.ERLANG, COMMENT), "%% Line comment\n");
        assertLine(Language.ERLANG, new Line(Language.ERLANG, COMMENT), "%\n");
        assertLine(Language.ERLANG, new Line(Language.ERLANG, CODE), "import Erlang.util.List; % with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.ERLANG, new Line(Language.ERLANG, BLANK), "     ");
        assertLine(Language.ERLANG, new Line(Language.ERLANG, BLANK), "\t");
        assertLine(Language.ERLANG, new Line(Language.ERLANG, CODE), "import Erlang.util.List;");
        assertLine(Language.ERLANG, new Line(Language.ERLANG, COMMENT), "%% Line comment");
        assertLine(Language.ERLANG, new Line(Language.ERLANG, COMMENT), "%");
        assertLine(Language.ERLANG, new Line(Language.ERLANG, CODE), "import Erlang.util.List; % with comment");
    }

    @Test
    public void helloWorld() {
        String code = "%% A type of hello world server request program\n"
                + "%% Written in Erlang\n"
                + "-module(hello).\n"
                + "-export([start/0]).\n"
                + "\n"
                + "start() ->\n"
                + "		spawn(fun() -> loop() end).\n"
                + "loop() ->\n"
                + "		receive % When receiving request print Hello, World!~\n"
                + "			hello ->\n"
                + "				io:format(\"Hello, World!~n\"),\n"
                + "				loop();\n"
                + "\t\n"
                + "			goodbye ->\n"
                + "				ok\n"
                + "		end.\n";

        Line[] expected = {
                new Line(Language.ERLANG, COMMENT),
                new Line(Language.ERLANG, COMMENT),
                new Line(Language.ERLANG, CODE),
                new Line(Language.ERLANG, CODE),
                new Line(Language.ERLANG, BLANK),
                new Line(Language.ERLANG, CODE),
                new Line(Language.ERLANG, CODE),
                new Line(Language.ERLANG, CODE),
                new Line(Language.ERLANG, CODE),
                new Line(Language.ERLANG, CODE),
                new Line(Language.ERLANG, CODE),
                new Line(Language.ERLANG, CODE),
                new Line(Language.ERLANG, BLANK),
                new Line(Language.ERLANG, CODE),
                new Line(Language.ERLANG, CODE),
                new Line(Language.ERLANG, CODE)
        };
        assertLines(Language.ERLANG, expected, code);
    }

}
