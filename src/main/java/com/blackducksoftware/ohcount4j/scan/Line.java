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

import com.blackducksoftware.ohcount4j.Entity;
import com.blackducksoftware.ohcount4j.Language;

public class Line {
    Language language;

    Entity entity;

    StringBuilder content;

    public Line() {
        content = new StringBuilder();
    }

    public Line(Language language) {
        this.language = language;
        content = new StringBuilder();
    }

    public Line(Language language, Entity entity) {
        this.language = language;
        this.entity = entity;
        content = new StringBuilder();
    }

    public Line(Language language, Entity entity, String content) {
        this.language = language;
        this.entity = entity;
        this.content = new StringBuilder(content);
    }

    @Override
    public String toString() {
        return String.format("%1$s\t%2$s\t%3$s", language.uname(), entity.name(), content);
    }

    public Language getLanguage() {
        return language;
    }

    public Line setLanguage(Language language) {
        this.language = language;
        return this;
    }

    public Entity getEntity() {
        return entity;
    }

    public Line setEntity(Entity entity) {
        this.entity = entity;
        return this;
    }

    public Line appendContent(char[] content) {
        this.content.append(content);
        return this;
    }

    public String getContent() {
        return content.toString();
    }

    public Line setContent(String content) {
        this.content = new StringBuilder(content);
        return this;
    }
}
