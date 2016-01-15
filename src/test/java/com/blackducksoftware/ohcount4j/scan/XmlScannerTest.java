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

public class XmlScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.XML, new Line(Language.XML, BLANK), "\n");
        assertLine(Language.XML, new Line(Language.XML, BLANK), "     \n");
        assertLine(Language.XML, new Line(Language.XML, BLANK), "\t\n");
        assertLine(Language.XML, new Line(Language.XML, CODE), "<taskdef resource=\"testngtasks\" classpath=\"${lib}/testng-6.3.1.jar\"/>\n");
        assertLine(Language.XML, new Line(Language.XML, COMMENT), "<!--comment-->\n");
        assertLine(Language.XML, new Line(Language.XML, CODE), "<property name=\"lib\" location=\"lib\"/> <!-- with comment -->\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.XML, new Line(Language.XML, BLANK), "     ");
        assertLine(Language.XML, new Line(Language.XML, BLANK), "\t");
        assertLine(Language.XML, new Line(Language.XML, CODE), "<taskdef resource=\"testngtasks\" classpath=\"${lib}/testng-6.3.1.jar\"/>");
        assertLine(Language.XML, new Line(Language.XML, COMMENT), "<!--comment-->");
        assertLine(Language.XML, new Line(Language.XML, CODE), "<property name=\"lib\" location=\"lib\"/> <!-- with comment -->");
    }

    @Test
    public void simpleTest() {
        String code = "<path id=\"lib.jars\"\n"
                + "<!--multi\n"
                + "\t\n"
                + "line comment -->\n"
                + "<fileset dir=\"${lib}\">\n"
                + "<include name=\"**/*.jar\"/>\n"
                + "</fileset>\n"
                + "\n"
                + "</path>";

        Line[] expected = {
                new Line(Language.XML, CODE),
                new Line(Language.XML, COMMENT),
                new Line(Language.XML, BLANK),
                new Line(Language.XML, COMMENT),
                new Line(Language.XML, CODE),
                new Line(Language.XML, CODE),
                new Line(Language.XML, CODE),
                new Line(Language.XML, BLANK),
                new Line(Language.XML, CODE)
        };
        assertLines(Language.XML, expected, code);
    }

    @Test
    public void cdataString() {
        // Everything inside a cdata string should be considered part of the string
        String code = "<!--This is a comment-->\n"
                + "<![CDATA[\n"
                + "<!--  this is a comment inside a cdata string-->\n"
                + "]]>\n"
                + "<!--<![CDATA[ This is a commented cdata string ]]>-->";
        Line[] expected = {
                new Line(Language.XML, COMMENT),
                new Line(Language.XML, CODE),
                new Line(Language.XML, CODE),
                new Line(Language.XML, CODE),
                new Line(Language.XML, COMMENT)
        };
        assertLines(Language.XML, expected, code);
    }
}
