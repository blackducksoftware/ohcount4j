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

import java.io.File;

import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.SourceFile;

public class HamlScannerTest extends BaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.HAML, new Line(Language.HAML, BLANK), "\n");
        assertLine(Language.HAML, new Line(Language.HAML, BLANK), "     \n");
        assertLine(Language.HAML, new Line(Language.HAML, CODE), "%p\n");
        assertLine(Language.HAML, new Line(Language.RUBY, CODE), "%p= \"stuff\"\n");
        assertLine(Language.HAML, new Line(Language.HAML, COMMENT), "-# silent comment\n");
        assertLine(Language.HAML, new Line(Language.HAML, CODE), "%a{:title => @title, :href => href} Stuff\n");
        assertLine(Language.HAML, new Line(Language.HAML, CODE), "%sandwich{hash1, hash2, :delicious => 'true'}/\n");
        assertLine(Language.HAML, new Line(Language.HAML, CODE), "%input(selected=true)\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.HAML, new Line(Language.HAML, BLANK), "     ");
        assertLine(Language.HAML, new Line(Language.HAML, CODE), "%p");
        assertLine(Language.HAML, new Line(Language.RUBY, CODE), "%p= \"stuff\"");
        assertLine(Language.HAML, new Line(Language.HAML, COMMENT), "-# silent comment");
        assertLine(Language.HAML, new Line(Language.HAML, CODE), "%a{:title => @title, :href => href} Stuff");
        assertLine(Language.HAML, new Line(Language.HAML, CODE), "%sandwich{hash1, hash2, :delicious => 'true'}/");
        assertLine(Language.HAML, new Line(Language.HAML, CODE), "%input(selected=true)");
    }

    @Test
    public void sampleTestWithSourceFile() throws Exception {
        SourceFile sourceFile = new SourceFile(new File(getSourceCodePath("haml.haml")));
        Line[] expected = {
                new Line(Language.HAML, CODE),
                new Line(Language.RUBY, CODE),
                new Line(Language.HAML, COMMENT),
                new Line(Language.HAML, CODE),
                new Line(Language.HAML, COMMENT),
                new Line(Language.HAML, COMMENT),
                new Line(Language.HAML, COMMENT),
                new Line(Language.RUBY, CODE),
                new Line(Language.HAML, CODE),
                new Line(Language.RUBY, CODE),
                new Line(Language.RUBY, CODE),
                new Line(Language.HAML, CODE),
                new Line(Language.HAML, BLANK),
                new Line(Language.HAML, CODE),
                new Line(Language.HAML, COMMENT),
                new Line(Language.HAML, CODE),
                new Line(Language.HAML, COMMENT),
                new Line(Language.HAML, COMMENT),
                new Line(Language.HAML, COMMENT),
                new Line(Language.HAML, CODE),
        };
        assertLines(Language.HAML, expected, sourceFile);
    }

}
