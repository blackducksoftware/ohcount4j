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

import static org.testng.AssertJUnit.assertEquals;

import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.scan.Line;

public class CountListTest {
    @Test
    public void getCountByLanguage() {
        Count l = new Count();
        l.add(new LanguageCount(Language.C, 1, 2, 3));
        l.add(new LanguageCount(Language.JAVA, 3, 4, 5));

        assertEquals(new LanguageCount(Language.C, 1, 2, 3), l.getLanguageCount(Language.C));
        assertEquals(new LanguageCount(Language.JAVA, 3, 4, 5), l.getLanguageCount(Language.JAVA));
        assertEquals(null, l.getLanguageCount(Language.RUBY));
    }

    @Test
    public void addCountToEmpty() {
        Count l = new Count();
        l.add(new LanguageCount(Language.C, 1, 2, 3));
        assertEquals(1, l.getLanguageCounts().size());
        assertEquals(new LanguageCount(Language.C, 1, 2, 3), l.getLanguageCounts().get(0));
    }

    @Test
    public void addCountSameLanguage() {
        Count l = new Count();
        l.add(new LanguageCount(Language.C, 1, 2, 3));
        l.add(new LanguageCount(Language.C, 3, 4, 5));
        assertEquals(1, l.getLanguageCounts().size());
        assertEquals(new LanguageCount(Language.C, 4, 6, 8), l.getLanguageCounts().get(0));
    }

    @Test
    public void addCountNewLanguage() {
        Count l = new Count();
        l.add(new LanguageCount(Language.C, 1, 2, 3));
        l.add(new LanguageCount(Language.JAVA, 3, 4, 5));
        assertEquals(2, l.getLanguageCounts().size());
    }

    @Test
    public void addCountList() {
        Count l = new Count();
        l.add(new LanguageCount(Language.C, 1, 2, 3));
        l.add(new LanguageCount(Language.JAVA, 3, 4, 5));

        Count addend = new Count();
        addend.add(new LanguageCount(Language.C, 10, 20, 30));
        addend.add(new LanguageCount(Language.RUBY, 7, 8, 9));

        l.add(addend);

        assertEquals(3, l.getLanguageCounts().size());
        assertEquals(new LanguageCount(Language.C, 11, 22, 33), l.getLanguageCount(Language.C));
        assertEquals(new LanguageCount(Language.JAVA, 3, 4, 5), l.getLanguageCount(Language.JAVA));
        assertEquals(new LanguageCount(Language.RUBY, 7, 8, 9), l.getLanguageCount(Language.RUBY));
    }

    @Test
    public void addLine() {
        Count l = new Count();
        l.add(new Line(Language.C, Entity.CODE));
        l.add(new Line(Language.C, Entity.CODE));
        l.add(new Line(Language.C, Entity.CODE));
        l.add(new Line(Language.C, Entity.COMMENT));
        l.add(new Line(Language.C, Entity.COMMENT));
        l.add(new Line(Language.C, Entity.BLANK));
        l.add(new Line(Language.JAVA, Entity.CODE));

        assertEquals(2, l.getLanguageCounts().size());
        assertEquals(new LanguageCount(Language.C, 3, 2, 1), l.getLanguageCount(Language.C));
        assertEquals(new LanguageCount(Language.JAVA, 1, 0, 0), l.getLanguageCount(Language.JAVA));
    }

    @Test
    public void total() {
        Count l = new Count();
        l.add(new LanguageCount(Language.C, 1, 2, 3));
        l.add(new LanguageCount(Language.JAVA, 3, 4, 5));

        assertEquals(4, l.getCode());
        assertEquals(6, l.getComment());
        assertEquals(8, l.getBlank());
        assertEquals(18, l.getTotal());
        assertEquals(0.6f, l.getCommentRatio());
    }

    @Test
    public void sortedCounts() {
        Count l = new Count();
        l.add(new LanguageCount(Language.C, 1, 2, 3));
        l.add(new LanguageCount(Language.JAVA, 6, 7, 8));
        l.add(new LanguageCount(Language.RUBY, 3, 4, 5));
        l.sort();
        assertEquals(Language.JAVA, l.getLanguageCounts().get(0).getLanguage());
        assertEquals(Language.RUBY, l.getLanguageCounts().get(1).getLanguage());
        assertEquals(Language.C, l.getLanguageCounts().get(2).getLanguage());
    }
}
