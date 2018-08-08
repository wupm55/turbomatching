模糊查询

```
MongoCollectionFindOne[coll, <|"a" -> <|"$regex" -> "good", "$options" -> "i"|>|>]
```

返回:

```

```

**模糊查询简介**

MongoDB查询条件可以使用正则表达式，从而实现模糊查询的功能。模糊查询可以使用$regex操作符或直接使用正则表达式对象。

| MySQL                                         | MongoDB                                |
| --------------------------------------------- | -------------------------------------- |
| select * from student where name like ’%joe%’ | db.student.find({name:{$regex:/joe/}}) |
| select * from student where name regexp ’joe’ | db.student.find({name:/joe/})          |

**$regex操作符的介绍**

MongoDB使用$regex操作符来设置匹配字符串的正则表达式，使用PCRE(Pert Compatible Regular Expression)作为正则表达式语言。

- regex操作符
  - {<field>:{$regex:/pattern/，$options:’<options>’}}
  - {<field>:{$regex:’pattern’，$options:’<options>’}}
  - {<field>:{$regex:/pattern/<options>}}

- 正则表达式对象
  - {<field>: /pattern/<options>}

$regex与正则表达式对象的区别:

- 在$in操作符中只能使用正则表达式对象，例如:{name:{$in:[/^joe/i,/^jack/}}
- 在使用隐式的$and操作符中，只能使用$regex，例如:{name:{$regex:/^jo/i, $nin:['john']}}
- 当option选项中包含X或S选项时，只能使用$regex，例如:{name:{$regex:/m.*line/,$options:"si"}}

**$regex操作符的使用**

$regex操作符中的option选项可以改变正则匹配的默认行为，它包括i, m, x以及S四个选项，其含义如下

- i 忽略大小写，{<field>{$regex/pattern/i}}，设置i选项后，模式中的字母会进行大小写不敏感匹配。
- m 多行匹配模式，{<field>{$regex/pattern/,$options:'m'}，m选项会更改^和$元字符的默认行为，分别使用与行的开头和结尾匹配，而不是与输入字符串的开头和结尾匹配。
- x 忽略非转义的空白字符，{<field>:{$regex:/pattern/,$options:'m'}，设置x选项后，正则表达式中的非转义的空白字符将被忽略，同时井号(#)被解释为注释的开头注，只能显式位于option选项中。
- s 单行匹配模式{<field>:{$regex:/pattern/,$options:'s'}，设置s选项后，会改变模式中的点号(.)元字符的默认行为，它会匹配所有字符，包括换行符(\n)，只能显式位于option选项中。

使用$regex操作符时，需要注意下面几个问题:

- i，m，x，s可以组合使用，例如:{name:{$regex:/j*k/,$options:"si"}}
- 在设置索弓}的字段上进行正则匹配可以提高查询速度，而且当正则表达式使用的是前缀表达式时，查询速度会进一步提高，例如:{name:{$regex: /^joe/}