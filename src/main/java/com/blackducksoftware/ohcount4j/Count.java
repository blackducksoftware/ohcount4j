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

package com.blackducksoftware.ohcount4j;

import java.util.ArrayList;
import java.util.Collections;

import com.blackducksoftware.ohcount4j.scan.Line;

// Accumulates the itemized count of code, comments etc. in any number of individual
// languages, as well as the grand total over all languages. Count is the basic result
// returned by a line count of one or more source files.
public class Count {

    protected int fileCount = 0;

    // Maintain individual results for each language seen
    protected ArrayList<LanguageCount> languageCounts = new ArrayList<LanguageCount>();

    public ArrayList<LanguageCount> getLanguageCounts() {
        return languageCounts;
    }

    // Returns the line count for an individual Language
    public LanguageCount getLanguageCount(Language l) {
        for (LanguageCount languageCount : languageCounts) {
            if (languageCount.getLanguage() == l) {
                return languageCount;
            }
        }
        return null;
    }

    public void add(LanguageCount addend) {
        for (LanguageCount languageCount : languageCounts) {
            if (addend.getLanguage() == languageCount.getLanguage()) {
                languageCount.add(addend);
                return;
            }
        }
        languageCounts.add(addend);
    }

    public void add(Count addend) {
        for (LanguageCount a : addend.getLanguageCounts()) {
            add(a);
        }
        fileCount += addend.getFileCount();
    }

    public void add(Line line) {
        add(new LanguageCount(line));
    }

    public void sort() {
        Collections.sort(languageCounts);
        Collections.reverse(languageCounts);
    }

    // Returns the grand total count of code lines across all languages.
    public int getCode() {
        int code = 0;
        for (LanguageCount languageCount : languageCounts) {
            code += languageCount.getCode();
        }
        return code;
    }

    // Returns the grand total count of comment lines across all languages.
    public int getComment() {
        int comment = 0;
        for (LanguageCount lc : languageCounts) {
            comment += lc.getComment();
        }
        return comment;
    }

    // Returns the grand total count of blank lines across all languages.
    public int getBlank() {
        int blank = 0;
        for (LanguageCount lc : languageCounts) {
            blank += lc.getBlank();
        }
        return blank;
    }

    // Returns the grand total count of all lines across all languages.
    public int getTotal() {
        int total = 0;
        for (LanguageCount lc : languageCounts) {
            total += lc.getCode() + lc.getComment() + lc.getBlank();
        }
        return total;
    }

    public float getCommentRatio() {
        if (getComment() == 0) {
            return 0.0f;
        } else {
            return (float) getComment() / (float) (getCode() + getComment());
        }
    }

    public int incrementFileCount() {
        fileCount += 1;
        return fileCount;
    }

    public int getFileCount() {
        return fileCount;
    }

    public void print() {
        System.out.println("                            Ohcount4j Line Count Summary");
        System.out.println();
        System.out.println("Language                  Files       Code    Comment  Comment %      Blank      Total");
        System.out.println("------------------------  -----  ---------  ---------  ---------  ---------  ---------");

        sort();

        for (LanguageCount lc : getLanguageCounts()) {
            System.out.format("%-24s %6d %10d %10d %9.1f%% %10d %10d\n",
                    lc.getLanguage().niceName(),
                    lc.getFileCount(),
                    lc.getCode(),
                    lc.getComment(),
                    lc.getCommentRatio() * 100.0f,
                    lc.getBlank(),
                    lc.getTotal());
        }

        System.out.println("------------------------  -----  ---------  ---------  ---------  ---------  ---------");
        System.out.format("%-24s %6d %10d %10d %9.1f%% %10d %10d\n",
                "Total",
                getFileCount(),
                getCode(),
                getComment(),
                getCommentRatio() * 100.0f,
                getBlank(),
                getTotal());
    }
}
