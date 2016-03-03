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

public class JspScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.JSP, new Line(Language.JSP, BLANK), "\n");
        assertLine(Language.JSP, new Line(Language.JSP, BLANK), "     \n");
        assertLine(Language.JSP, new Line(Language.JSP, BLANK), "\t\n");
        assertLine(Language.JSP, new Line(Language.JSP, CODE), "</HTML>\n");
        assertLine(Language.JSP, new Line(Language.JSP, COMMENT), "<!-- comment -->\n");
        assertLine(Language.JSP, new Line(Language.JSP, CODE), "</HTML><!-- with comment -->\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.JSP, new Line(Language.JSP, BLANK), "     ");
        assertLine(Language.JSP, new Line(Language.JSP, BLANK), "\t");
        assertLine(Language.JSP, new Line(Language.JSP, CODE), "</HTML>");
        assertLine(Language.JSP, new Line(Language.JSP, COMMENT), "<!-- comment -->");
        assertLine(Language.JSP, new Line(Language.JSP, CODE), "</HTML><!-- with comment -->");
    }

    @Test
    public void helloWorld() {
        String code = "<HTML>\n"
                + "<HTML lang='en'>\n"
                + "<!-- A comment -->\n"
                + "<body>\n"
                + "\n"
                + "<h1>Hello, world!</h1>\n"
                + "\n"
                + "</body>\n"
                + "</HTML>";

        Line[] expected = {
                new Line(Language.JSP, CODE),
                new Line(Language.JSP, CODE),
                new Line(Language.JSP, COMMENT),
                new Line(Language.JSP, CODE),
                new Line(Language.JSP, BLANK),
                new Line(Language.JSP, CODE),
                new Line(Language.JSP, BLANK),
                new Line(Language.JSP, CODE),
                new Line(Language.JSP, CODE)
        };
        assertLines(Language.JSP, expected, code);
    }

    @Test
    public void embeddedCSSOnSeparateLine() {
        String code = "<HTML>\n"
                + "<style>\n"
                + "  body:after { content:\"Hello, world!\"; }\n"
                + "</style>\n"
                + "</HTML>";

        Line[] expected = {
                new Line(Language.JSP, CODE),
                new Line(Language.JSP, CODE),
                new Line(Language.CSS, CODE),
                new Line(Language.JSP, CODE),
                new Line(Language.JSP, CODE)
        };
        assertLines(Language.JSP, expected, code);
    }

    @Test
    public void embeddedCSSOnSameLine() {
        String code = "<HTML>\n"
                + "<style> body:after { content:\"Hello, world!\"; } </style>\n"
                + "</HTML>";

        Line[] expected = {
                new Line(Language.JSP, CODE),
                new Line(Language.CSS, CODE),
                new Line(Language.JSP, CODE)
        };
        assertLines(Language.JSP, expected, code);
    }

    @Test
    public void emptyCSSOnSameLine() {
        String code = "<HTML>\n"
                + "<style></style>\n"
                + "</HTML>";

        Line[] expected = {
                new Line(Language.JSP, CODE),
                new Line(Language.JSP, CODE),
                new Line(Language.JSP, CODE)
        };
        assertLines(Language.JSP, expected, code);
    }

    @Test
    public void commentCSSOnSameLine() {
        String code = "<HTML>\n"
                + "<style>/* No code just comment */</style>\n"
                + "</HTML>";

        Line[] expected = {
                new Line(Language.JSP, CODE),
                new Line(Language.CSS, COMMENT),
                new Line(Language.JSP, CODE)
        };
        assertLines(Language.JSP, expected, code);
    }

    @Test
    public void embeddedJavaScriptOnSeparateLine() {
        String code = "<HTML>\n"
                + "<script type=\"script/javascript\">\n"
                + "  document.write(\"Hello, world!\\n\");\n"
                + "</script>\n"
                + "</HTML>";

        Line[] expected = {
                new Line(Language.JSP, CODE),
                new Line(Language.JSP, CODE),
                new Line(Language.JAVASCRIPT, CODE),
                new Line(Language.JSP, CODE),
                new Line(Language.JSP, CODE)
        };
        assertLines(Language.JSP, expected, code);
    }

    @Test
    public void embeddedJavaScriptOnSameLine() {
        String code = "<HTML>\n"
                + "<script type=\"script/javascript\">document.write(\"Hello, world!\\n\");</script>\n"
                + "</HTML>";

        Line[] expected = {
                new Line(Language.JSP, CODE),
                new Line(Language.JAVASCRIPT, CODE),
                new Line(Language.JSP, CODE)
        };
        assertLines(Language.JSP, expected, code);
    }

    @Test
    public void embeddedJavaOnSeparateLine() {
        String code = "<HTML>\n"
                + "<%!\n"
                + "  String message = \"Hello, World, from JSP\";\n"
                + "%>\n"
                + "</HTML>";

        Line[] expected = {
                new Line(Language.JSP, CODE),
                new Line(Language.JSP, CODE),
                new Line(Language.JAVA, CODE),
                new Line(Language.JSP, CODE),
                new Line(Language.JSP, CODE)
        };
        assertLines(Language.JSP, expected, code);
    }

    @Test
    public void embeddedJavaOnSameLine() {
        String code = "<HTML>\n"
                + "<% new java.util.Date() %>\n"
                + "</HTML>";

        Line[] expected = {
                new Line(Language.JSP, CODE),
                new Line(Language.JAVA, CODE),
                new Line(Language.JSP, CODE)
        };
        assertLines(Language.JSP, expected, code);
    }

    @Test
    public void commentJavaOnSameLine() {
        String code = "<HTML>\n"
                + "<% /* No code just comment */ %>\n"
                + "</HTML>";

        Line[] expected = {
                new Line(Language.JSP, CODE),
                new Line(Language.JAVA, COMMENT),
                new Line(Language.JSP, CODE)
        };
        assertLines(Language.JSP, expected, code);
    }

}
