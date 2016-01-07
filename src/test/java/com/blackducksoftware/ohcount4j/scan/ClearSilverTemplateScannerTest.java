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

import static com.blackducksoftware.ohcount4j.Entity.CODE;
import static com.blackducksoftware.ohcount4j.Entity.COMMENT;

import java.io.File;

import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.SourceFile;

/**
 * @author gandhip
 *
 */
public class ClearSilverTemplateScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.CLEARSILVER, new Line(Language.CLEARSILVER, CODE), "<p><?cs if:?error ?></p>\n");
        assertLine(Language.CLEARSILVER, new Line(Language.CLEARSILVER, COMMENT), "<p><?cs #comment ?></p>\n");
        assertLine(Language.CLEARSILVER, new Line(Language.CLEARSILVER, CODE), "<?cs include:\"templates/header.cs\" ?>\n");
        assertLine(Language.CLEARSILVER, new Line(Language.HTML, CODE), "<form method=\"post\"> \n");
        assertLine(Language.CLEARSILVER, new Line(Language.HTML, COMMENT), "<!-- comment -->\n");
        assertLine(Language.CLEARSILVER, new Line(Language.CSS, CODE), "<style> body:after { content:\"Hello, world!\"; } </style>\n");
        assertLine(Language.CLEARSILVER, new Line(Language.JAVASCRIPT, CODE),
                "<script type=\"script/javascript\">document.write(\"Hello, world!\\n\");</script>\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.CLEARSILVER, new Line(Language.CLEARSILVER, CODE), "<p><?cs if:?error ?></p>");
        assertLine(Language.CLEARSILVER, new Line(Language.CLEARSILVER, COMMENT), "<p><?cs #comment ?></p>");
        assertLine(Language.CLEARSILVER, new Line(Language.CLEARSILVER, CODE), "<?cs include:\"templates/header.cs\" ?>");
        assertLine(Language.CLEARSILVER, new Line(Language.HTML, CODE), "<form method=\"post\"> ");
        assertLine(Language.CLEARSILVER, new Line(Language.HTML, COMMENT), "<!-- comment -->");
        assertLine(Language.CLEARSILVER, new Line(Language.CSS, CODE), "<style> body:after { content:\"Hello, world!\"; } </style>");
        assertLine(Language.CLEARSILVER, new Line(Language.JAVASCRIPT, CODE),
                "<script type=\"script/javascript\">document.write(\"Hello, world!\");</script>");
    }

    @Test
    public void sampleTestWithSourceFile() throws Exception {
        SourceFile sourceFile = new SourceFile(new File(getSourceCodePath("clearsilver_template1.cs")));
        Line[] expected = {
                new Line(Language.HTML, CODE),
                new Line(Language.HTML, CODE),
                new Line(Language.JAVASCRIPT, CODE),
                new Line(Language.CLEARSILVER, COMMENT),
                new Line(Language.CLEARSILVER, CODE),
                new Line(Language.HTML, COMMENT),
                new Line(Language.HTML, COMMENT),
                new Line(Language.HTML, CODE),
                new Line(Language.HTML, CODE)
        };
        assertLines(Language.CLEARSILVER, expected, sourceFile);
    }

    @Test
    public void sampleTestWithRealTimeSourceFile() throws Exception {
        SourceFile sourceFile = new SourceFile(new File(getSourceCodePath("clearsilver_template_realtime.cs")));
        Line[] expected = {
                new Line(Language.CLEARSILVER, CODE),
                new Line(Language.HTML, CODE),
                new Line(Language.CLEARSILVER, CODE),
                new Line(Language.CLEARSILVER, CODE),
                new Line(Language.CLEARSILVER, CODE),
                new Line(Language.HTML, CODE),
                new Line(Language.HTML, CODE),
                new Line(Language.HTML, CODE),
                new Line(Language.CLEARSILVER, CODE),
                new Line(Language.HTML, CODE),
                new Line(Language.HTML, CODE),
                new Line(Language.HTML, CODE),
                new Line(Language.CLEARSILVER, CODE),
                new Line(Language.HTML, CODE),
                new Line(Language.HTML, CODE),
                new Line(Language.HTML, CODE),
                new Line(Language.HTML, CODE),
                new Line(Language.HTML, CODE),
                new Line(Language.HTML, CODE),
                new Line(Language.HTML, CODE),
                new Line(Language.HTML, CODE),
                new Line(Language.HTML, CODE),
                new Line(Language.HTML, CODE),
                new Line(Language.CLEARSILVER, CODE),
        };
        assertLines(Language.CLEARSILVER, expected, sourceFile);
    }

}
