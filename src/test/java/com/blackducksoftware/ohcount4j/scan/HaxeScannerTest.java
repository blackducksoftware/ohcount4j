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

import java.io.File;

import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.SourceFile;

/**
 * @author gandhip
 *
 */
public class HaxeScannerTest extends BaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.HAXE, new Line(Language.HAXE, BLANK), "\n");
        assertLine(Language.HAXE, new Line(Language.HAXE, BLANK), "     \n");
        assertLine(Language.HAXE, new Line(Language.HAXE, BLANK), "\t\n");
        assertLine(Language.HAXE, new Line(Language.HAXE, CODE), "package main.package;\n");
        assertLine(Language.HAXE, new Line(Language.HAXE, CODE), "class Main {\n");
        assertLine(Language.HAXE, new Line(Language.HAXE, CODE), "void main(){\n");
        assertLine(Language.HAXE, new Line(Language.HAXE, COMMENT), "// Line comment\n");
        assertLine(Language.HAXE, new Line(Language.HAXE, COMMENT), "/* Block Comment */\n");
        assertLine(Language.HAXE, new Line(Language.HAXE, CODE), "public static var COLLADA:String = \"DAE\";\n");
        assertLine(Language.HAXE, new Line(Language.HAXE, CODE), "public static function create( p_sFile:Dynamic ):IParser\n");
        assertLine(Language.HAXE, new Line(Language.HAXE, CODE), "if ( p_nScale == null ) p_nScale = 1;\n");
    }

    @Test
    public void helloWorld() {
        String code = "/* Hello World\n"
                + " * with multi-line comment */\n"
                + "\n"
                + "package main.package;\n"
                + "\n"
                + "class Main {\n"
                + "// single line comment\n"
                + "\t\tstatic public function main():Void {\n"
                + "\t\ttrace(\"Hello World\");\n"
                + "\t}\n"
                + "\t}\n";
        Line[] expected = {
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, BLANK),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, BLANK),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, CODE),
        };
        assertLines(Language.HAXE, expected, code);
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.HAXE, new Line(Language.HAXE, BLANK), "     ");
        assertLine(Language.HAXE, new Line(Language.HAXE, BLANK), "\t");
        assertLine(Language.HAXE, new Line(Language.HAXE, CODE), "package main.package;");
        assertLine(Language.HAXE, new Line(Language.HAXE, CODE), "class Main {");
        assertLine(Language.HAXE, new Line(Language.HAXE, CODE), "void main(){");
        assertLine(Language.HAXE, new Line(Language.HAXE, COMMENT), "// Line comment");
        assertLine(Language.HAXE, new Line(Language.HAXE, COMMENT), "/* Block Comment */");
        assertLine(Language.HAXE, new Line(Language.HAXE, CODE), "public static var COLLADA:String = \"DAE\";");
        assertLine(Language.HAXE, new Line(Language.HAXE, CODE), "public static function create( p_sFile:Dynamic ):IParser");
        assertLine(Language.HAXE, new Line(Language.HAXE, CODE), "if ( p_nScale == null ) p_nScale = 1;");
    }

    @Test
    public void sampleTestWithSourceFile() throws Exception {
        SourceFile sourceFile = new SourceFile(new File(getSourceCodePath("haxe1.hx")));
        Line[] expected = {
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, BLANK),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, BLANK),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, BLANK),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, BLANK),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, BLANK),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, BLANK),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, COMMENT),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, CODE),
                new Line(Language.HAXE, BLANK),
        };
        assertLines(Language.HAXE, expected, sourceFile);
    }
}
