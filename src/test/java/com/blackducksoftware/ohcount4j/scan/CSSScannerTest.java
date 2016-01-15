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

public class CSSScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.CSS, new Line(Language.CSS, BLANK), "\n");
        assertLine(Language.CSS, new Line(Language.CSS, BLANK), "     \n");
        assertLine(Language.CSS, new Line(Language.CSS, BLANK), "\t\n");
        assertLine(Language.CSS, new Line(Language.CSS, CODE), "margin: 1em;\n");
        assertLine(Language.CSS, new Line(Language.CSS, COMMENT), "/* comment */\n");
        assertLine(Language.CSS, new Line(Language.CSS, CODE), "margin: 1em; /* with comment */\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.CSS, new Line(Language.CSS, BLANK), "     ");
        assertLine(Language.CSS, new Line(Language.CSS, BLANK), "\t");
        assertLine(Language.CSS, new Line(Language.CSS, CODE), "margin: 1em;");
        assertLine(Language.CSS, new Line(Language.CSS, COMMENT), "/* comment */");
        assertLine(Language.CSS, new Line(Language.CSS, CODE), "margin: 1em; /* with comment */");
    }

    @Test
    public void helloWorld() {
        String code = "/* Pure CSS Hello World */\n"
                + "\n"
                + "body:after {\n"
                + "  content:\"Hello, world!\";\n"
                + "}";

        Line[] expected = {
                new Line(Language.CSS, COMMENT),
                new Line(Language.CSS, BLANK),
                new Line(Language.CSS, CODE),
                new Line(Language.CSS, CODE),
                new Line(Language.CSS, CODE)
        };
        assertLines(Language.CSS, expected, code);
    }
}
