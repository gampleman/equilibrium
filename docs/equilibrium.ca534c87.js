parcelRequire=function(e,r,t,n){var i,o="function"==typeof parcelRequire&&parcelRequire,u="function"==typeof require&&require;function f(t,n){if(!r[t]){if(!e[t]){var i="function"==typeof parcelRequire&&parcelRequire;if(!n&&i)return i(t,!0);if(o)return o(t,!0);if(u&&"string"==typeof t)return u(t);var c=new Error("Cannot find module '"+t+"'");throw c.code="MODULE_NOT_FOUND",c}p.resolve=function(r){return e[t][1][r]||r},p.cache={};var l=r[t]=new f.Module(t);e[t][0].call(l.exports,p,l,l.exports,this)}return r[t].exports;function p(e){return f(p.resolve(e))}}f.isParcelRequire=!0,f.Module=function(e){this.id=e,this.bundle=f,this.exports={}},f.modules=e,f.cache=r,f.parent=o,f.register=function(r,t){e[r]=[function(e,r){r.exports=t},{}]};for(var c=0;c<t.length;c++)try{f(t[c])}catch(e){i||(i=e)}if(t.length){var l=f(t[t.length-1]);"object"==typeof exports&&"undefined"!=typeof module?module.exports=l:"function"==typeof define&&define.amd?define(function(){return l}):n&&(this[n]=l)}if(parcelRequire=f,i)throw i;return f}({"3oS9":[function(require,module,exports) {
!function(n){"use strict";function r(n,r,t){return t.a=n,t.f=r,t}function t(n){return r(2,n,function(r){return function(t){return n(r,t)}})}function e(n){return r(3,n,function(r){return function(t){return function(e){return n(r,t,e)}}})}function u(n){return r(4,n,function(r){return function(t){return function(e){return function(u){return n(r,t,e,u)}}}})}function i(n){return r(5,n,function(r){return function(t){return function(e){return function(u){return function(i){return n(r,t,e,u,i)}}}}})}function f(n){return r(6,n,function(r){return function(t){return function(e){return function(u){return function(i){return function(f){return n(r,t,e,u,i,f)}}}}}})}function a(n){return r(7,n,function(r){return function(t){return function(e){return function(u){return function(i){return function(f){return function(a){return n(r,t,e,u,i,f,a)}}}}}}})}function o(n,r,t){return 2===n.a?n.f(r,t):n(r)(t)}function c(n,r,t,e){return 3===n.a?n.f(r,t,e):n(r)(t)(e)}function v(n,r,t,e,u){return 4===n.a?n.f(r,t,e,u):n(r)(t)(e)(u)}function b(n,r,t,e,u,i){return 5===n.a?n.f(r,t,e,u,i):n(r)(t)(e)(u)(i)}function s(n,r,t,e,u,i,f){return 6===n.a?n.f(r,t,e,u,i,f):n(r)(t)(e)(u)(i)(f)}function l(n,r,t,e,u,i,f,a){return 7===n.a?n.f(r,t,e,u,i,f,a):n(r)(t)(e)(u)(i)(f)(a)}var d={$:0};function h(n,r){return{$:1,a:n,b:r}}var $=t(h);function g(n){for(var r=d,t=n.length;t--;)r=h(n[t],r);return r}function m(n,r){for(var t,e=[],u=p(n,r,0,e);u&&(t=e.pop());u=p(t.a,t.b,0,e));return u}function p(n,r,t,e){if(t>100)return e.push(k(n,r)),!0;if(n===r)return!0;if("object"!=typeof n||null===n||null===r)return"function"==typeof n&&L(5),!1;for(var u in 0>n.$&&(n=gr(n),r=gr(r)),n)if(!p(n[u],r[u],t+1,e))return!1;return!0}function y(n,r,t){if("object"!=typeof n)return n===r?0:r>n?-1:1;if(void 0===n.$)return(t=y(n.a,r.a))?t:(t=y(n.b,r.b))?t:y(n.c,r.c);for(;n.b&&r.b&&!(t=y(n.a,r.a));n=n.b,r=r.b);return t||(n.b?1:r.b?-1:0)}var w=t(function(n,r){var t=y(n,r);return 0>t?dr:t?lr:sr});function k(n,r){return{a:n,b:r}}function j(n,r,t){return{a:n,b:r,c:t}}function A(n,r){var t={};for(var e in n)t[e]=n[e];for(var e in r)t[e]=r[e];return t}var _=e(function(n,r,t){for(var e=[],u=0;n>u;u++)e[u]=t(r+u);return e}),F=t(function(n,r){for(var t=[],e=0;n>e&&r.b;e++)t[e]=r.a,r=r.b;return t.length=e,k(t,r)}),N=t(function(n,r){return r[n]}),T=e(function(n,r,t){for(var e=t.length,u=[],i=0;e>i;i++)u[i]=t[i];return u[n]=r,u}),x=t(function(n,r){for(var t=r.length,e=[],u=0;t>u;u++)e[u]=r[u];return e[t]=n,e}),E=e(function(n,r,t){for(var e=t.length,u=0;e>u;u++)r=o(n,t[u],r);return r});function L(n){throw Error("https://github.com/elm/core/blob/1.0.0/hints/"+n+".md")}var S=t(function(n,r){return n+r}),q=t(Math.pow),C=t(function(n,r){var t=r%n;return 0===n?L(11):t>0&&0>n||0>t&&n>0?t+n:t}),Y=Math.ceil,B=Math.floor,O=Math.round,R=Math.log,z=t(function(n,r){return r.join(n)});function G(n){return n+""}function K(n){return{$:2,b:n}}var D=K(function(n){return"number"!=typeof n?tn("an INT",n):n>-2147483647&&2147483647>n&&(0|n)===n?Dr(n):!isFinite(n)||n%1?tn("an INT",n):Dr(n)}),I=K(function(n){return"boolean"==typeof n?Dr(n):tn("a BOOL",n)}),M=K(function(n){return"number"==typeof n?Dr(n):tn("a FLOAT",n)});K(function(n){return Dr(fn(n))}),K(function(n){return"string"==typeof n?Dr(n):n instanceof String?Dr(n+""):tn("a STRING",n)});var X=t(function(n,r){return{$:6,d:n,b:r}});function J(n,r){return{$:9,f:n,g:r}}var P=t(function(n,r){return J(n,[r])}),Q=e(function(n,r,t){return J(n,[r,t])}),W=u(function(n,r,t,e){return J(n,[r,t,e])}),H=a(function(n,r,t,e,u,i,f){return J(n,[r,t,e,u,i,f])}),V=t(function(n,r){return U(n,an(r))});function U(n,r){switch(n.$){case 2:return n.b(r);case 5:return null===r?Dr(n.c):tn("null",r);case 3:return nn(r)?Z(n.b,r,g):tn("a LIST",r);case 4:return nn(r)?Z(n.b,r,rn):tn("an ARRAY",r);case 6:var t=n.d;if("object"!=typeof r||null===r||!(t in r))return tn("an OBJECT with a field named `"+t+"`",r);var e=U(n.b,r[t]);return br(e)?e:Kr(o(Mr,t,e.a));case 7:var u=n.e;return nn(r)?r.length>u?(e=U(n.b,r[u]),br(e)?e:Kr(o(Xr,u,e.a))):tn("a LONGER array. Need index "+u+" but only see "+r.length+" entries",r):tn("an ARRAY",r);case 8:if("object"!=typeof r||null===r||nn(r))return tn("an OBJECT",r);var i=d;for(var f in r)if(r.hasOwnProperty(f)){if(e=U(n.b,r[f]),!br(e))return Kr(o(Mr,f,e.a));i=h(k(f,e.a),i)}return Dr(Tr(i));case 9:for(var a=n.f,c=n.g,v=0;c.length>v;v++){if(e=U(c[v],r),!br(e))return e;a=a(e.a)}return Dr(a);case 10:return e=U(n.b,r),br(e)?U(n.h(e.a),r):e;case 11:for(var b=d,s=n.g;s.b;s=s.b){if(e=U(s.a,r),br(e))return e;b=h(e.a,b)}return Kr(Jr(Tr(b)));case 1:return Kr(o(Ir,n.a,fn(r)));case 0:return Dr(n.a)}}function Z(n,r,t){for(var e=r.length,u=[],i=0;e>i;i++){var f=U(n,r[i]);if(!br(f))return Kr(o(Xr,i,f.a));u[i]=f.a}return Dr(t(u))}function nn(n){return Array.isArray(n)||"undefined"!=typeof FileList&&n instanceof FileList}function rn(n){return o(Rr,n.length,function(r){return n[r]})}function tn(n,r){return Kr(o(Ir,"Expecting "+n,fn(r)))}function en(n,r){if(n===r)return!0;if(n.$!==r.$)return!1;switch(n.$){case 0:case 1:return n.a===r.a;case 2:return n.b===r.b;case 5:return n.c===r.c;case 3:case 4:case 8:return en(n.b,r.b);case 6:return n.d===r.d&&en(n.b,r.b);case 7:return n.e===r.e&&en(n.b,r.b);case 9:return n.f===r.f&&un(n.g,r.g);case 10:return n.h===r.h&&en(n.b,r.b);case 11:return un(n.g,r.g)}}function un(n,r){var t=n.length;if(t!==r.length)return!1;for(var e=0;t>e;e++)if(!en(n[e],r[e]))return!1;return!0}function fn(n){return n}function an(n){return n}function on(n){return{$:0,a:n}}function cn(n){return{$:2,b:n,c:null}}fn(null);var vn=t(function(n,r){return{$:3,b:n,d:r}}),bn=0;function sn(n){var r={$:0,e:bn++,f:n,g:null,h:[]};return mn(r),r}function ln(n){return cn(function(r){r(on(sn(n)))})}function dn(n,r){n.h.push(r),mn(n)}var hn=t(function(n,r){return cn(function(t){dn(n,r),t(on(0))})}),$n=!1,gn=[];function mn(n){if(gn.push(n),!$n){for($n=!0;n=gn.shift();)pn(n);$n=!1}}function pn(n){for(;n.f;){var r=n.f.$;if(0===r||1===r){for(;n.g&&n.g.$!==r;)n.g=n.g.i;if(!n.g)return;n.f=n.g.b(n.f.a),n.g=n.g.i}else{if(2===r)return void(n.f.c=n.f.b(function(r){n.f=r,mn(n)}));if(5===r){if(0===n.h.length)return;n.f=n.f.b(n.h.shift())}else n.g={$:3===r?0:1,b:n.f.b,i:n.g},n.f=n.f.d}}}var yn={};function wn(n,r,t,e,u){return{b:n,c:r,d:t,e:e,f:u}}function kn(n,r){var t={g:r,h:void 0},e=n.c,u=n.d,i=n.e,f=n.f;function a(n){return o(vn,a,{$:5,b:function(r){var a=r.a;return 0===r.$?c(u,t,a,n):i&&f?v(e,t,a.i,a.j,n):c(e,t,i?a.i:a.j,n)}})}return t.h=sn(o(vn,a,n.b))}var jn=t(function(n,r){return cn(function(t){n.g(r),t(on(0))})}),An=t(function(n,r){return o(hn,n.h,{$:0,a:r})});function _n(n){return function(r){return{$:1,k:n,l:r}}}function Fn(n,r,t){var e={};for(var u in Nn(!0,r,e,null),Nn(!1,t,e,null),n)dn(n[u],{$:"fx",a:e[u]||{i:d,j:d}})}function Nn(n,r,t,e){switch(r.$){case 1:var u=r.k,i=function(n,t,e){function u(n){for(var r=e;r;r=r.q)n=r.p(n);return n}return o(n?yn[t].e:yn[t].f,u,r.l)}(n,u,e);return void(t[u]=function(n,r,t){return t=t||{i:d,j:d},n?t.i=h(r,t.i):t.j=h(r,t.j),t}(n,i,t[u]));case 2:for(var f=r.m;f.b;f=f.b)Nn(n,f.a,t,e);return;case 3:return void Nn(n,r.o,t,{p:r.n,q:e})}}var Tn,xn=t(function(n,r){return cn(function(){var t=setInterval(function(){sn(r)},n);return function(){clearInterval(t)}})}),En="undefined"!=typeof document?document:{};function Ln(n,r){n.appendChild(r)}function Sn(n){return{$:0,a:n}}var qn=t(function(n,r){return t(function(t,e){for(var u=[],i=0;e.b;e=e.b){var f=e.a;i+=f.b||0,u.push(f)}return i+=u.length,{$:1,c:r,d:zn(t),e:u,f:n,b:i}})}),Cn=qn(void 0);t(function(n,r){return t(function(t,e){for(var u=[],i=0;e.b;e=e.b){var f=e.a;i+=f.b.b||0,u.push(f)}return i+=u.length,{$:2,c:r,d:zn(t),e:u,f:n,b:i}})})(void 0);var Yn,Bn=t(function(n,r){return{$:4,j:n,k:r,b:1+(r.b||0)}}),On=t(function(n,r){return{$:"a0",n:n,o:r}}),Rn=t(function(n,r){return{$:"a3",n:n,o:r}});function zn(n){for(var r={};n.b;n=n.b){var t=n.a,e=t.$,u=t.n,i=t.o;if("a2"!==e){var f=r[e]||(r[e]={});"a3"===e&&"class"===u?Gn(f,u,i):f[u]=i}else"className"===u?Gn(r,u,an(i)):r[u]=an(i)}return r}function Gn(n,r,t){var e=n[r];n[r]=e?e+" "+t:t}function Kn(n,r){var t=n.$;if(5===t)return Kn(n.k||(n.k=n.m()),r);if(0===t)return En.createTextNode(n.a);if(4===t){for(var e=n.k,u=n.j;4===e.$;)"object"!=typeof u?u=[u,e.j]:u.push(e.j),e=e.k;var i={j:u,p:r};return(f=Kn(e,i)).elm_event_node_ref=i,f}if(3===t)return Dn(f=n.h(n.g),r,n.d),f;var f=n.f?En.createElementNS(n.f,n.c):En.createElement(n.c);Tn&&"a"==n.c&&f.addEventListener("click",Tn(f)),Dn(f,r,n.d);for(var a=n.e,o=0;a.length>o;o++)Ln(f,Kn(1===t?a[o]:a[o].b,r));return f}function Dn(n,r,t){for(var e in t){var u=t[e];"a1"===e?In(n,u):"a0"===e?Jn(n,r,u):"a3"===e?Mn(n,u):"a4"===e?Xn(n,u):("value"!==e&&"checked"!==e||n[e]!==u)&&(n[e]=u)}}function In(n,r){var t=n.style;for(var e in r)t[e]=r[e]}function Mn(n,r){for(var t in r){var e=r[t];void 0!==e?n.setAttribute(t,e):n.removeAttribute(t)}}function Xn(n,r){for(var t in r){var e=r[t],u=e.f,i=e.o;void 0!==i?n.setAttributeNS(u,t,i):n.removeAttributeNS(u,t)}}function Jn(n,r,t){var e=n.elmFs||(n.elmFs={});for(var u in t){var i=t[u],f=e[u];if(i){if(f){if(f.q.$===i.$){f.q=i;continue}n.removeEventListener(u,f)}f=Pn(r,i),n.addEventListener(u,f,Yn&&{passive:2>su(i)}),e[u]=f}else n.removeEventListener(u,f),e[u]=void 0}}try{window.addEventListener("t",null,Object.defineProperty({},"passive",{get:function(){Yn=!0}}))}catch(n){}function Pn(n,r){function t(r){var e=t.q,u=U(e.a,r);if(br(u)){for(var i,f=su(e),a=u.a,o=f?3>f?a.a:a.T:a,c=1==f?a.b:3==f&&a.bS,v=(c&&r.stopPropagation(),(2==f?a.b:3==f&&a.bK)&&r.preventDefault(),n);i=v.j;){if("function"==typeof i)o=i(o);else for(var b=i.length;b--;)o=i[b](o);v=v.p}v(o,c)}}return t.q=r,t}function Qn(n,r){return n.$==r.$&&en(n.a,r.a)}function Wn(n,r,t,e){var u={$:r,r:t,s:e,t:void 0,u:void 0};return n.push(u),u}function Hn(n,r,t,e){if(n!==r){var u=n.$,i=r.$;if(u!==i){if(1!==u||2!==i)return void Wn(t,0,e,r);r=function(n){for(var r=n.e,t=r.length,e=[],u=0;t>u;u++)e[u]=r[u].b;return{$:1,c:n.c,d:n.d,e:e,f:n.f,b:n.b}}(r),i=1}switch(i){case 5:for(var f=n.l,a=r.l,o=f.length,c=o===a.length;c&&o--;)c=f[o]===a[o];if(c)return void(r.k=n.k);r.k=r.m();var v=[];return Hn(n.k,r.k,v,0),void(v.length>0&&Wn(t,1,e,v));case 4:for(var b=n.j,s=r.j,l=!1,d=n.k;4===d.$;)l=!0,"object"!=typeof b?b=[b,d.j]:b.push(d.j),d=d.k;for(var h=r.k;4===h.$;)l=!0,"object"!=typeof s?s=[s,h.j]:s.push(h.j),h=h.k;return l&&b.length!==s.length?void Wn(t,0,e,r):((l?function(n,r){for(var t=0;n.length>t;t++)if(n[t]!==r[t])return!1;return!0}(b,s):b===s)||Wn(t,2,e,s),void Hn(d,h,t,e+1));case 0:return void(n.a!==r.a&&Wn(t,3,e,r.a));case 1:return void Vn(n,r,t,e,Zn);case 2:return void Vn(n,r,t,e,nr);case 3:if(n.h!==r.h)return void Wn(t,0,e,r);var $=Un(n.d,r.d);$&&Wn(t,4,e,$);var g=r.i(n.g,r.g);return void(g&&Wn(t,5,e,g))}}}function Vn(n,r,t,e,u){if(n.c===r.c&&n.f===r.f){var i=Un(n.d,r.d);i&&Wn(t,4,e,i),u(n,r,t,e)}else Wn(t,0,e,r)}function Un(n,r,t){var e;for(var u in n)if("a1"!==u&&"a0"!==u&&"a3"!==u&&"a4"!==u)if(u in r){var i=n[u],f=r[u];i===f&&"value"!==u&&"checked"!==u||"a0"===t&&Qn(i,f)||((e=e||{})[u]=f)}else(e=e||{})[u]=t?"a1"===t?"":"a0"===t||"a3"===t?void 0:{f:n[u].f,o:void 0}:"string"==typeof n[u]?"":null;else{var a=Un(n[u],r[u]||{},u);a&&((e=e||{})[u]=a)}for(var o in r)o in n||((e=e||{})[o]=r[o]);return e}function Zn(n,r,t,e){var u=n.e,i=r.e,f=u.length,a=i.length;f>a?Wn(t,6,e,{v:a,i:f-a}):a>f&&Wn(t,7,e,{v:f,e:i});for(var o=a>f?f:a,c=0;o>c;c++){var v=u[c];Hn(v,i[c],t,++e),e+=v.b||0}}function nr(n,r,t,e){for(var u=[],i={},f=[],a=n.e,o=r.e,c=a.length,v=o.length,b=0,s=0,l=e;c>b&&v>s;){var d=(F=a[b]).a,h=(N=o[s]).a,$=F.b,g=N.b,m=void 0,p=void 0;if(d!==h){var y=a[b+1],w=o[s+1];if(y){var k=y.a,j=y.b;p=h===k}if(w){var A=w.a,_=w.b;m=d===A}if(m&&p)Hn($,_,u,++l),tr(i,u,d,g,s,f),l+=$.b||0,er(i,u,d,j,++l),l+=j.b||0,b+=2,s+=2;else if(m)l++,tr(i,u,h,g,s,f),Hn($,_,u,l),l+=$.b||0,b+=1,s+=2;else if(p)er(i,u,d,$,++l),l+=$.b||0,Hn(j,g,u,++l),l+=j.b||0,b+=2,s+=1;else{if(!y||k!==A)break;er(i,u,d,$,++l),tr(i,u,h,g,s,f),l+=$.b||0,Hn(j,_,u,++l),l+=j.b||0,b+=2,s+=2}}else Hn($,g,u,++l),l+=$.b||0,b++,s++}for(;c>b;){var F;er(i,u,(F=a[b]).a,$=F.b,++l),l+=$.b||0,b++}for(;v>s;){var N,T=T||[];tr(i,u,(N=o[s]).a,N.b,void 0,T),s++}(u.length>0||f.length>0||T)&&Wn(t,8,e,{w:u,x:f,y:T})}var rr="_elmW6BL";function tr(n,r,t,e,u,i){var f=n[t];if(!f)return i.push({r:u,A:f={c:0,z:e,r:u,s:void 0}}),void(n[t]=f);if(1===f.c){i.push({r:u,A:f}),f.c=2;var a=[];return Hn(f.z,e,a,f.r),f.r=u,void(f.s.s={w:a,A:f})}tr(n,r,t+rr,e,u,i)}function er(n,r,t,e,u){var i=n[t];if(i){if(0===i.c){i.c=2;var f=[];return Hn(e,i.z,f,u),void Wn(r,9,u,{w:f,A:i})}er(n,r,t+rr,e,u)}else{var a=Wn(r,9,u,void 0);n[t]={c:1,z:e,r:u,s:a}}}function ur(n,r,t,e){return 0===t.length?n:(function n(r,t,e,u){!function r(t,e,u,i,f,a,o){for(var c=u[i],v=c.r;v===f;){var b=c.$;if(1===b)n(t,e.k,c.s,o);else if(8===b)c.t=t,c.u=o,(s=c.s.w).length>0&&r(t,e,s,0,f,a,o);else if(9===b){c.t=t,c.u=o;var s,l=c.s;l&&(l.A.s=t,(s=l.w).length>0&&r(t,e,s,0,f,a,o))}else c.t=t,c.u=o;if(!(c=u[++i])||(v=c.r)>a)return i}var d=e.$;if(4===d){for(var h=e.k;4===h.$;)h=h.k;return r(t,h,u,i,f+1,a,t.elm_event_node_ref)}for(var $=e.e,g=t.childNodes,m=0;$.length>m;m++){var p=1===d?$[m]:$[m].b,y=++f+(p.b||0);if(!(f>v||v>y||(c=u[i=r(g[m],p,u,i,f,y,o)])&&(v=c.r)<=a))return i;f=y}return i}(r,t,e,0,0,t.b,u)}(n,r,t,e),ir(n,t))}function ir(n,r){for(var t=0;r.length>t;t++){var e=r[t],u=e.t,i=fr(u,e);u===n&&(n=i)}return n}function fr(n,r){switch(r.$){case 0:return function(n){var t=n.parentNode,e=Kn(r.s,r.u);return e.elm_event_node_ref||(e.elm_event_node_ref=n.elm_event_node_ref),t&&e!==n&&t.replaceChild(e,n),e}(n);case 4:return Dn(n,r.u,r.s),n;case 3:return n.replaceData(0,n.length,r.s),n;case 1:return ir(n,r.s);case 2:return n.elm_event_node_ref?n.elm_event_node_ref.j=r.s:n.elm_event_node_ref={j:r.s,p:r.u},n;case 6:for(var t=r.s,e=0;t.i>e;e++)n.removeChild(n.childNodes[t.v]);return n;case 7:for(var u=(t=r.s).e,i=n.childNodes[e=t.v];u.length>e;e++)n.insertBefore(Kn(u[e],r.u),i);return n;case 9:if(!(t=r.s))return n.parentNode.removeChild(n),n;var f=t.A;return void 0!==f.r&&n.parentNode.removeChild(n),f.s=ir(n,t.w),n;case 8:return function(n,r){var t=r.s,e=function(n,r){if(n){for(var t=En.createDocumentFragment(),e=0;n.length>e;e++){var u=n[e].A;Ln(t,2===u.c?u.s:Kn(u.z,r.u))}return t}}(t.y,r);n=ir(n,t.w);for(var u=t.x,i=0;u.length>i;i++){var f=u[i],a=f.A,o=2===a.c?a.s:Kn(a.z,r.u);n.insertBefore(o,n.childNodes[f.r])}return e&&Ln(n,e),n}(n,r);case 5:return r.s(n);default:L(10)}}var ar=u(function(n,r,t,e){return function(n,r,t,e,u,i){var f=o(V,n,fn(r?r.flags:void 0));br(f)||L(2);var a={},c=(f=t(f.a)).a,v=i(s,c),b=function(n,r){var t;for(var e in yn){var u=yn[e];u.a&&((t=t||{})[e]=u.a(e,r)),n[e]=kn(u,r)}return t}(a,s);function s(n,r){v(c=(f=o(e,n,c)).a,r),Fn(a,f.b,u(c))}return Fn(a,f.b,u(c)),b?{ports:b}:{}}(r,e,n.dl,n.dQ,n.dJ,function(r,t){var u=n.dS,i=e.node,f=function n(r){if(3===r.nodeType)return Sn(r.textContent);if(1!==r.nodeType)return Sn("");for(var t=d,e=r.attributes,u=e.length;u--;){var i=e[u];t=h(o(Rn,i.name,i.value),t)}var f=r.tagName.toLowerCase(),a=d,v=r.childNodes;for(u=v.length;u--;)a=h(n(v[u]),a);return c(Cn,f,t,a)}(i);return function(n,r){r(n);var t=0;function e(){t=1===t?0:(or(e),r(n),1)}return function(u,i){n=u,i?(r(n),2===t&&(t=1)):(0===t&&or(e),t=2)}}(t,function(n){var t=u(n),e=function(n,r){var t=[];return Hn(n,r,t,0),t}(f,t);i=ur(i,f,e,r),f=t})})}),or=("undefined"!=typeof cancelAnimationFrame&&cancelAnimationFrame,"undefined"!=typeof requestAnimationFrame?requestAnimationFrame:function(n){return setTimeout(n,1e3/60)});"undefined"!=typeof document&&document,"undefined"!=typeof window&&window;var cr,vr={$:0},br=function(n){return!n.$},sr=1,lr=2,dr=0,hr=e(function(n,r,t){for(;;){if(-2===t.$)return r;var e=t.d,u=n,i=c(n,t.b,t.c,c(hr,n,r,t.e));n=u,r=i,t=e}}),$r=$,gr=function(n){return c(hr,e(function(n,r,t){return o($r,k(n,r),t)}),d,n)},mr=u(function(n,r,t,e){return{$:0,a:n,b:r,c:t,d:e}}),pr=Y,yr=t(function(n,r){return R(r)/R(n)}),wr=pr(o(yr,2,32)),kr=[],jr=v(mr,0,wr,kr,kr),Ar=function(n){return{$:1,a:n}},_r=function(n){return{$:0,a:n}},Fr=F,Nr=e(function(n,r,t){for(;;){if(!t.b)return r;var e=t.b,u=n,i=o(n,t.a,r);n=u,r=i,t=e}}),Tr=function(n){return c(Nr,$r,d,n)},xr=t(function(n,r){for(;;){var t=o(Fr,32,n),e=t.b,u=o($r,_r(t.a),r);if(!e.b)return Tr(u);n=e,r=u}}),Er=t(function(n,r){for(;;){var t=pr(r/32);if(1===t)return o(Fr,32,n).a;n=o(xr,n,d),r=t}}),Lr=S,Sr=B,qr=t(function(n,r){return y(n,r)>0?n:r}),Cr=function(n){return n.length},Yr=t(function(n,r){if(r.i){var t=32*r.i,e=Sr(o(yr,32,t-1)),u=n?Tr(r.m):r.m,i=o(Er,u,r.i);return v(mr,Cr(r.l)+t,o(qr,5,e*wr),i,r.l)}return v(mr,Cr(r.l),wr,kr,r.l)}),Br=_,Or=i(function(n,r,t,e,u){for(;;){if(0>r)return o(Yr,!1,{m:e,i:t/32|0,l:u});var i=Ar(c(Br,32,r,n));n=n,r-=32,t=t,e=o($r,i,e),u=u}}),Rr=t(function(n,r){if(n>0){var t=n%32;return b(Or,r,n-t-32,n,d,c(Br,t,n-t,r))}return jr}),zr=function(n){return{$:0,a:n}},Gr={$:1},Kr=function(n){return{$:1,a:n}},Dr=function(n){return{$:0,a:n}},Ir=t(function(n,r){return{$:3,a:n,b:r}}),Mr=t(function(n,r){return{$:0,a:n,b:r}}),Xr=t(function(n,r){return{$:1,a:n,b:r}}),Jr=function(n){return{$:2,a:n}},Pr=function(n){return c(Nr,t(function(n,r){return r+1}),0,n)},Qr=e(function(n,r,t){for(;;){if(y(n,r)>=1)return t;var e=n,u=r-1,i=o($r,r,t);n=e,r=u,t=i}}),Wr=t(function(n,r){return c(Qr,n,r,d)}),Hr=G,Vr=t(function(n,r){return o(z,n,function(n){for(var r=[];n.b;n=n.b)r.push(n.a);return r}(r))}),Ur={$:2,m:d},Zr={$:2},nt=t(function(n){return n}),rt=t(function(n,r){return{$:0,a:n,b:r}}),tt={$:-2},et=tt,ut=on,it=t(function(n,r){return{cD:r,cL:n}}),ft=ut(o(it,et,et)),at=i(function(n,r,t,e,u){return{$:-1,a:n,b:r,c:t,d:e,e:u}}),ot=w,ct=i(function(n,r,t,e,u){if(-1!==u.$||u.a){if(-1!==e.$||e.a||-1!==e.d.$||e.d.a)return b(at,n,r,t,e,u);var i=e.d;return f=e.e,b(at,0,e.b,e.c,b(at,1,i.b,i.c,i.d,i.e),b(at,1,r,t,f,u))}var f,a=u.b,o=u.c,c=u.d,v=u.e;return-1!==e.$||e.a?b(at,n,a,o,b(at,0,r,t,e,c),v):b(at,0,r,t,b(at,1,e.b,e.c,e.d,f=e.e),b(at,1,a,o,c,v))}),vt=e(function(n,r,t){if(-2===t.$)return b(at,0,n,r,tt,tt);var e=t.a,u=t.b,i=t.c,f=t.d,a=t.e;switch(o(ot,n,u)){case 0:return b(ct,e,u,i,c(vt,n,r,f),a);case 1:return b(at,e,u,r,f,a);default:return b(ct,e,u,i,f,c(vt,n,r,a))}}),bt=e(function(n,r,t){var e=c(vt,n,r,t);return-1!==e.$||e.a?e:b(at,1,e.b,e.c,e.d,e.e)}),st=e(function(n,r,t){for(;;){if(-2===t.$)return r;var e=t.e,u=n,i=c(n,t.b,t.c,c(st,n,r,t.d));n=u,r=i,t=e}}),lt=f(function(n,r,u,i,f,a){var o=c(st,e(function(t,e,i){n:for(;;){var f=i.a,a=i.b;if(f.b){var o=f.a,b=o.a,s=o.b,l=f.b;if(0>y(b,t)){t=t,e=e,i=k(l,c(n,b,s,a));continue n}return y(b,t)>0?k(f,c(u,t,e,a)):k(l,v(r,b,s,e,a))}return k(f,c(u,t,e,a))}}),k(gr(i),a),f),b=o.a,s=o.b;return c(Nr,t(function(r,t){return c(n,r.a,r.b,t)}),s,b)}),dt=vn,ht=t(function(n,r){n:for(;;){if(-2===r.$)return Gr;var t=r.c,e=r.d,u=r.e;switch(o(ot,n,r.b)){case 0:n=n,r=e;continue n;case 1:return zr(t);default:n=n,r=u;continue n}}}),$t=t(function(n,r){var t=n.a,e=n.b,u=o(ht,t,r);return c(bt,t,1===u.$?g([e]):o($r,e,u.a),r)}),gt=An,mt=ln,pt=xn,yt=e(function(n,r,t){if(r.b){var e=r.a,u=r.b,i=mt(o(pt,e,o(gt,n,e)));return o(dt,function(r){return c(yt,n,u,c(bt,e,r,t))},i)}return ut(t)}),wt=e(function(n,r,t){var i=t.cD,f=e(function(n,r,t){var e,u=t.c;return j(t.a,t.b,o(dt,function(){return u},(e=r,cn(function(n){var r=e.f;2===r.$&&r.c&&r.c(),e.f=null,n(on(0))}))))}),a=c(Nr,$t,et,r),v=s(lt,e(function(n,r,t){var e=t.b,u=t.c;return j(o($r,n,t.a),e,u)}),u(function(n,r,t,e){var u=e.c;return j(e.a,c(bt,n,t,e.b),u)}),f,a,i,j(d,et,ut(0))),b=v.a,l=v.b;return o(dt,function(n){return ut(o(it,a,n))},o(dt,function(){return c(yt,n,b,l)},v.c))}),kt=u(function(n,r,t,e){if(e.b){var u=e.a,i=e.b;if(i.b){var f=i.a,a=i.b;if(a.b){var b=a.a,s=a.b;if(s.b){var l=s.b;return o(n,u,o(n,f,o(n,b,o(n,s.a,t>500?c(Nr,n,r,Tr(l)):v(kt,n,r,t+1,l)))))}return o(n,u,o(n,f,o(n,b,r)))}return o(n,u,o(n,f,r))}return o(n,u,r)}return r}),jt=e(function(n,r,t){return v(kt,n,r,0,t)}),At=t(function(n,r){return c(jt,t(function(r,t){return o($r,n(r),t)}),d,r)}),_t=jn,Ft=e(function(n,r,t){return o(dt,function(r){return o(dt,function(t){return ut(o(n,r,t))},t)},r)}),Nt=function(n){return c(jt,Ft($r),ut(d),n)},Tt=(cr=function(n){return n},cn(function(n){n(on(cr(Date.now())))})),xt=e(function(n,r,t){var e=o(ht,r,t.cL);if(1===e.$)return ut(t);var u=e.a;return o(dt,function(){return ut(t)},o(dt,function(r){return Nt(o(At,function(t){return o(_t,n,t(r))},u))},Tt))}),Et=e(function(n,r,t){return n(r(t))});yn.Time=wn(ft,wt,xt,0,t(function(n,r){return o(rt,r.a,o(Et,n,r.b))}));var Lt=_n("Time"),St=t(function(n,r){return Lt(o(rt,n,r))}),qt=function(n){return{$:1,a:n}},Ct=function(n){return{$:1,a:n}},Yt=function(n){return n?0:1},Bt=t(function(n,r){return r.$?n:r.a}),Ot=4294967295>>>32-wr,Rt=N,zt=e(function(n,r,t){for(;;){var e=o(Rt,Ot&r>>>n,t);if(e.$)return o(Rt,Ot&r,e.a);n-=wr,r=r,t=e.a}}),Gt=function(n){return n>>>5<<5},Kt=t(function(n,r){var t=r.a,e=r.b,u=r.c,i=r.d;return 0>n||y(n,t)>-1?Gr:y(n,Gt(t))>-1?zr(o(Rt,Ot&n,i)):zr(c(zt,e,n,u))}),Dt=t(function(n,r){return n*r.a+r.b}),It=t(function(n,r){var t=r.b;return o(Kt,o(Dt,r.a.a,k(n.a,n.b)),t)}),Mt=T,Xt=u(function(n,r,t,e){var u=Ot&r>>>n,i=o(Rt,u,e);return c(Mt,u,i.$?Ar(c(Mt,Ot&r,t,i.a)):_r(v(Xt,n-wr,r,t,i.a)),e)}),Jt=e(function(n,r,t){var e=t.a,u=t.b,i=t.c,f=t.d;return 0>n||y(n,e)>-1?t:y(n,Gt(e))>-1?v(mr,e,u,i,c(Mt,Ot&n,r,f)):v(mr,e,u,v(Xt,u,n,r,i),f)}),Pt=t(function(n,r){return{$:0,a:n,b:r}}),Qt=e(function(n,r,t){var e=n.a,u=e.a,i=e.b,f=n.b,a=o(Dt,u,k(r.a,r.b));return o(Pt,k(u,i),c(Jt,a,t,f))}),Wt=t(function(n,r){return c(Qt,r,n,Yt(o(Bt,0,o(It,n,r))))}),Ht=q,Vt=u(function(n,r,t,e){for(;;){if(1>r)return k(n,e);var u=t(e),i=u.b;n=o($r,u.a,n),r-=1,t=t,e=i}}),Ut=t(function(n,r){var t=r;return function(r){return v(Vt,d,n,t,r)}}),Zt=t(function(n,r){var t=r;return function(r){var e=t(r),u=e.b;return k(n(e.a),u)}}),ne=function(n){return 0>n?-n:n},re=t(function(n,r){return{$:0,a:n,b:r}}),te=function(n){var r=n.b;return o(re,1664525*n.a+r>>>0,r)},ee=function(n){var r=n.a,t=277803737*(r^r>>>4+(r>>>28));return(t>>>22^t)>>>0},ue=t(function(n,r){return function(t){var e=te(t),u=ne(r-n),i=ee(e);return k((1*(67108863&ee(t))*134217728+1*(134217727&i))/9007199254740992*u+n,te(e))}}),ie=e(function(n,r,t){for(;;){var e=n.a,u=n.b;if(!r.b)return u;var i=r.a,f=r.b;if(1>y(t,ne(e)))return u;n=i,r=f,t-=ne(e)}}),fe=t(function(n,r){var t=function(n){return ne(n.a)},e=t(n)+c(Nr,Lr,0,o(At,t,r));return o(Zt,o(ie,n,r),o(ue,0,e))}),ae=e(function(n,r,t){for(;;){var e=o(Fr,32,n),u=e.a,i=e.b;if(0>y(Cr(u),32))return o(Yr,!0,{m:r,i:t,l:u});n=i,r=o($r,Ar(u),r),t+=1}}),oe=function(n){return n.b?c(ae,n,d,0):jr},ce=o(Pt,k(0,0),oe(d)),ve=e(function(n,r,t){return m(Pr(t),n*r)?zr(o(Pt,k(n,r),oe(t))):Gr}),be=o(Zt,function(n){return o(Bt,ce,c(ve,30,30,n))},o(Ut,o(Ht,30,2),o(fe,k(.55,0),g([k(.5,1)])))),se=function(n){var r=te(o(re,0,1013904223));return te(o(re,r.a+n>>>0,r.b))},le=t(function(n,r){return n(r)}),de={F:(3445768976564,o(le,be,se(3445768976564)).a),aG:10,av:0},he=E,$e=e(function(n,r,e){var u=e.c,i=e.d,f=t(function(r,t){return c(he,r.$?n:f,t,r.a)});return c(he,n,c(he,f,r,u),i)}),ge=e(function(n,r,t){return c($e,n,r,t.b)}),me=o(ge,t(function(n,r){return A(r,n?{al:r.al+1}:{ao:r.ao+1})}),{al:0,ao:0}),pe={b3:Sr(.4*o(Ht,30,2)),cb:Sr(.02*o(Ht,30,2))},ye=function(n){var r=n.aG,t=me(n.F),e=ne(t.al-t.ao);return 1>y(e,pe.cb)?r+1:y(e,pe.b3)>-1?r-1:r},we=t(function(n,r){return 0>y(n,r)?n:r}),ke=t(function(n,r){return r.b?c(jt,$r,r,n):n}),je=t(function(n,r){return c(jt,ke,d,o(At,n,r))}),Ae=t(function(n,r){return k(n,r)}),_e=u(function(n,r,e,u){var i=me(n),f=y(i.ao,i.al)>0?0:1;return c(Nr,t(function(n,r){return c(Qt,r,n,f)}),n,o(je,function(n){return o(At,Ae(n),o(Wr,o(qr,0,e-u),o(we,30,e+u)))},o(Wr,o(qr,0,r-u),o(we,30,r+u))))}),Fe=t(function(n,r){var t=r;return function(r){var e=t(r),u=e.b;return n(e.a)(u)}}),Ne=function(n){return function(r){return k(n,r)}},Te=t(function(n,r){return function(t){var e=0>y(n,r)?k(n,r):k(r,n),u=e.a,i=e.b-u+1;if(i-1&i){var f=(-i>>>0)%i>>>0;return function(n){for(;;){var r=ee(n),t=te(n);if(y(r,f)>=0)return k(r%i+u,t);n=t}}(t)}return k(((i-1&ee(t))>>>0)+u,te(t))}}),xe=u(function(n,r,t,e){var u=r,i=t,f=e;return function(r){var t=u(r),e=t.a,a=i(t.b),o=a.a,v=f(a.b),b=v.b;return k(c(n,e,o,v.a),b)}}),Ee=function(n){return o(Fe,function(r){return r?v(xe,_e(n),o(Te,0,29),o(Te,0,29),o(Te,5,Sr(7.5))):Ne(n)},o(fe,k(20,!1),g([k(1,!0)])))},Le=e(function(n,r,t){var e=n(r);return e.$?t:o($r,e.a,t)}),Se=t(function(n,r){return c(jt,Le(n),d,r)}),qe=t(function(n,r){var t=n.a,e=n.b;return o(Se,function(n){return o(It,n,r)},g([k(t-1,e-1),k(t,e-1),k(t+1,e-1),k(t-1,e),k(t+1,e),k(t-1,e+1),k(t,e+1),k(t+1,e+1)]))}),Ce=function(n){return!n},Ye=t(function(n,r){for(;;){if(!r.b)return!1;var t=r.b;if(n(r.a))return!0;n=n,r=t}}),Be=t(function(n,r){return!o(Ye,o(Et,Ce,n),r)}),Oe=e(function(n,r,t){return Ne(o(Be,function(r){return!m(n,r)},o(qe,r,t))?Yt(n):n)}),Re=e(function(n,r,t){return r(n(t))}),ze=e(function(n,r,t){var e=r,u=t;return function(r){var t=e(r),i=t.a,f=u(t.b),a=f.b;return k(o(n,i,f.a),a)}}),Ge=x,Ke=function(n){return[n]},De=u(function(n,r,t,e){var u=Ot&r>>>n;if(y(u,Cr(e))>-1){if(5===n)return o(Ge,Ar(t),e);var i=_r(v(De,n-wr,r,t,kr));return o(Ge,i,e)}var f=o(Rt,u,e);if(f.$)return i=_r(v(De,n-wr,r,t,Ke(f))),c(Mt,u,i,e);i=_r(v(De,n-wr,r,t,f.a));return c(Mt,u,i,e)}),Ie=t(function(n,r){var t=r.a,e=r.b,u=r.c,i=Cr(r.d),f=Cr(n),a=t+(f-i);if(m(f,32)){if(y(a>>>wr,1<<e)>0){var o=e+wr,c=v(De,o,t,n,Ke(_r(u)));return v(mr,a,o,c,kr)}return v(mr,a,e,v(De,e,t,n,u),kr)}return v(mr,a,e,u,n)}),Me=t(function(n,r){return o(Ie,o(Ge,n,r.d),r)}),Xe=t(function(n,r){var t=r.a,e=r.b;return 0>y(e,n.a-1)?k(t,e+1):k(t+1,0)}),Je=t(function(n,r){var e=r.a,u=e.a,i=e.b,f=r.b,a=t(function(t,e){var f=e.a,a=f.a,c=f.b,v=e.b;return k(o(Xe,k(u,i),k(a,c)),o(Me,o(n,k(a,c),r),v))});return o(Pt,k(u,i),c($e,a,k(k(0,0),jr),f).b)}),Pe=t(function(n,r){return function(n){var r=n.a;return o(Zt,o(Re,o(ve,r.a,r.b),Bt(ce)),o(Zt,Tr,c(ge,t(function(n,r){return c(ze,$r,n,r)}),Ne(d),n)))}(o(Je,t(function(t,e){return c(n,o(Bt,0,o(It,t,e)),t,r)}),r))}),Qe=t(function(n,r){return c(jt,t(function(r,t){return n(r)?o($r,r,t):t}),d,r)}),We=e(function(n,r,t){var e=o(qe,r,t),u=Pr(o(Qe,function(r){return!m(n,r)},e));return o(fe,k(u,Yt(n)),g([k(15*Pr(e),n)]))}),He=function(n){return o(Fe,Ee,o(Fe,Pe(We),o(Pe,Oe,n)))},Ve=o(dt,function(n){return ut(se(n))},Tt),Ue=e(function(n,r,t){if(r.b){var e=r.b,u=o(le,r.a,t),i=u.b;return o(dt,function(){return c(Ue,n,e,i)},o(_t,n,u.a))}return ut(t)});yn.Random=wn(Ve,Ue,e(function(n,r,t){return ut(t)}),t(function(n,r){return o(Zt,n,r)}));var Ze,nu=_n("Random"),ru=t(function(n,r){return nu(o(Zt,n,r))}),tu=t(function(n,r){var t=k(n,r);n:for(;;)switch(t.b.$){case 0:case 2:if(3===t.a.$)return k(Ct(de),Ur);break n;default:switch(t.a.$){case 0:return k(Ct(A(e=t.b.a,{F:o(Wt,t.a.a,e.F)})),Ur);case 2:return k(Ct(A(e=t.b.a,{av:e.av+1})),o(ru,qt,He(e.F)));case 1:var e,u=t.a.a,i=ye(e=t.b.a);return k(i>0?Ct(A(e,{F:u,aG:i})):{$:2,a:e.av},Ur);default:break n}}return k(r,Ur)}),eu={$:3},uu=u(function(n,r,t,e){return{$:0,a:n,b:r,c:t,d:e}}),iu=e(function(n,r,t){return v(uu,n,r,t,1)}),fu=c(iu,.3,.3,.3),au=c(iu,.1,.9,.1),ou={cW:function(n){return n?fu:au},cX:20,c3:c(iu,0,0,0),dd:c(iu,0,0,1),de:0},cu=P,vu=Q,bu=function(n){return{$:0,a:n}},su=function(n){switch(n.$){case 0:return 0;case 1:return 1;case 2:return 2;default:return 3}},lu=Cn("button"),du=Cn("div"),hu=Bn,$u=Cn("p"),gu=Sn,mu=On,pu=t(function(n,r){return o(mu,n,{$:0,a:r})}),yu=function(n){return o(pu,"click",bu(n))},wu=qn(function(n){return n}("http://www.w3.org/2000/svg")),ku=wu("line"),ju=wu("rect"),Au=wu("svg"),_u=t(function(n,r){return o(Rn,function(n){return/^(on|formAction$)/i.test(n)?"data-"+n:n}(n),function(n){return/^\s*(javascript:|data:text\/html)/i.test(n)?"":n}(r))}),Fu=O,Nu=G,Tu=function(n){var r,t,e=n.b,u=n.c,i=n.d,f=function(n){return Fu(1e4*n)/100};return r=g(["rgba(",Nu(f(n.a)),"%,",Nu(f(e)),"%,",Nu(f(u)),"%,",Nu((t=i,Fu(1e3*t)/1e3)),")"]),o(Vr,"",r)},xu=function(n){return n.$?"none":Tu(n.a)},Eu=o(Et,_u("fill"),xu),Lu=function(n){return o(_u,"stroke",Tu(n))},Su=function(n){switch(n.$){case 0:return Nu(n.a)+"cm";case 1:return Nu(n.a)+"em";case 2:return Nu(n.a)+"ex";case 3:return Nu(n.a)+"in";case 4:return Nu(n.a)+"mm";case 5:return Nu(n.a);case 6:return Nu(n.a)+"pc";case 7:return Nu(n.a)+"%";case 8:return Nu(n.a)+"pt";default:return Nu(n.a)+"px"}},qu=function(n){return{$:9,a:n}},Cu=function(n){return r=qu(n),o(_u,"height",Su(r));var r},Yu=function(n){return r=qu(n),o(_u,"width",Su(r));var r},Bu=function(n){return r=qu(n),o(_u,"x",Su(r));var r},Ou=function(n){return r=qu(n),o(_u,"x1",Su(r));var r},Ru=function(n){return r=qu(n),o(_u,"x2",Su(r));var r},zu=function(n){return r=qu(n),o(_u,"y1",Su(r));var r},Gu=function(n){return r=qu(n),o(_u,"y2",Su(r));var r},Ku=function(n){return{$:0,a:n}},Du=u(function(n,r,t,e){return o(_u,"viewBox",o(Vr," ",o(At,Nu,g([n,r,t,e]))))}),Iu=wu("g"),Mu=C,Xu=t(function(n,r){return k(r/n.b|0,o(Mu,n.a,r))}),Ju=t(function(n,r){return r.$?Gr:zr(n(r.a))}),Pu=function(n){return r=qu(n),o(_u,"stroke-width",Su(r));var r},Qu=t(function(n,r){return{$:0,a:n,b:r}}),Wu={bK:!0,bS:!1},Hu=t(function(n,r){return o(mu,n,{$:3,a:r})}),Vu=H,Uu=f(function(n,r,t,e,u,i){return{cV:r,c$:t,dn:n,dv:e,dz:u,dG:i}}),Zu=X,ni=o(cu,function(n){switch(n){case 0:return 1;case 1:return 2;case 2:return 3;case 3:return 4;case 4:return 5;default:return 0}},o(Zu,"button",D)),ri=M,ti=c(vu,t(function(n,r){return k(n,r)}),o(Zu,"clientX",ri),o(Zu,"clientY",ri)),ei=I,ui=l(Vu,Uu,v(W,e(function(n,r,t){return{cQ:n,c1:r,dH:t}}),o(Zu,"altKey",ei),o(Zu,"ctrlKey",ei),o(Zu,"shiftKey",ei)),ni,ti,c(vu,t(function(n,r){return k(n,r)}),o(Zu,"offsetX",ri),o(Zu,"offsetY",ri)),c(vu,t(function(n,r){return k(n,r)}),o(Zu,"pageX",ri),o(Zu,"pageY",ri)),c(vu,t(function(n,r){return k(n,r)}),o(Zu,"screenX",ri),o(Zu,"screenY",ri))),ii=o(e(function(n,r,t){return o(Hu,n,o(cu,function(n){return{T:t(n),bK:r.bK,bS:r.bS}},ui))}),"mousedown",Wu),fi=e(function(n,r,t){var e,u,i=t.a,f=t.b,a=n.cX,c=o(Bt,n.c3,o(Ju,n.cW,o(It,k(i,f),r)));return o(ju,g([Yu(a),Cu(a),Bu(a*i),(e=a*f,u=qu(e),o(_u,"y",Su(u))),Eu(Ku(c)),ii(o(Re,function(n){return n.c$},Qu(k(i,f)))),Pu(n.de),Lu(n.dd)]),d)}),ai=t(function(n,r){return o(Iu,d,o(At,o(fi,n,r),(i=(e=(t=r.a).a)*(u=t.b),o(At,Xu(k(e,u)),o(Wr,0,i-1)))));var t,e,u,i}),oi=u(function(n,r,t,e){return o(Au,g([Cu(r),Yu(n),v(Du,0,0,n,r)]),g([o(ai,t,e)]))}),ci=ut(0),vi=t(function(n,r){return o(dt,function(r){return ut(n(r))},r)}),bi=t(function(n,r){var t=r;return ln(o(dt,_t(n),t))});yn.Task=wn(ci,e(function(n,r){return o(vi,function(){return 0},Nt(o(At,bi(n),r)))}),e(function(){return ut(0)}),t(function(n,r){return o(vi,n,r)})),_n("Task"),Ze={Main:{init:ar({dl:function(){return k(vr,Ur)},dJ:function(){return o(St,1e3,nt(Zr))},dQ:tu,dS:function(n){switch(n.$){case 0:return o(du,d,g([o($u,d,g([gu("Your goal is to keep the ecosystem in a state of equilibrium. As long as there is perfect balance, your health will increase. But extreme imbalance will cause you to wither, until you die. Try to live as long as possible.")])),o(lu,g([yu(eu)]),g([gu("Start")]))]));case 2:var r=n.a;return o(du,d,g([o($u,d,g([gu("You died! You survived for  "),gu(Hr(r)),gu(" seconds.")])),o(lu,g([yu(eu)]),g([gu("Try again")]))]));default:var t=n.a,e=me(t.F),u=e.ao,i=e.al;return o(du,d,g([o(hu,function(n){return{$:0,a:n.a}},v(oi,600,600,ou,t.F)),o($u,d,g([gu("Health "),gu(Hr(t.aG))])),o(Au,g([Yu(400),Cu(30)]),g([o(ju,g([Bu(0),Yu(u/o(Ht,30,2)*400),Cu(20),Eu(Ku(au))]),d),o(ju,g([Bu(u/o(Ht,30,2)*400),Yu(i/o(Ht,30,2)*400),Cu(20),Eu(Ku(fu))]),d),o(ku,g([Ou(200),Ru(200),zu(21),Gu(30),Lu(c(iu,0,0,0))]),d),o(ku,g([Ou(120),Ru(120),zu(21),Gu(30),Lu(c(iu,.9,0,0))]),d),o(ku,g([Ou(280),Ru(280),zu(21),Gu(30),Lu(c(iu,.9,0,0))]),d)])),o($u,d,g([gu(Hr(t.av))]))]))}}})(bu({}))(0)}},n.Elm?function n(r,t){for(var e in t)e in r?"init"==e?L(6):n(r[e],t[e]):r[e]=t[e]}(n.Elm,Ze):n.Elm=Ze}(this);
},{}],"Focm":[function(require,module,exports) {
"use strict";var e=require("./src/Main.elm");e.Elm.Main.init({node:document.body});
},{"./src/Main.elm":"3oS9"}]},{},["Focm"], null)
//# sourceMappingURL=equilibrium.ca534c87.js.map