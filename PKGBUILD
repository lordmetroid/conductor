# Maintainer: Nicklas W Bjurman <nicklas.w.bjurman@monkeyfactory.org>
pkgname=Conductor
pkgver=1
pkgrel=1
pkgdesc="Web application server"
arch=('any')
url="http://www.monkeyfactory.org"
license=('WTFPL')
depends=('erlang-nox')
optdepends=('symphony: strict view template')
backup=(etc/conductor/server.conf)
source=("https://github.com$pkgname-$pkgver.tar.gz"
        "$pkgname-$pkgver.patch")
noextract=()
md5sums=()
validpgpkeys=()

prepare() {
	cd "$pkgname-$pkgver"
	patch -p1 -i "$srcdir/$pkgname-$pkgver.patch"
}

build() {
	cd "$pkgname-$pkgver"
	./configure --prefix=/usr
	make
}

check() {
	cd "$pkgname-$pkgver"
	make -k check
}

package() {
	cd "$pkgname-$pkgver"
	make DESTDIR="$pkgdir/" install
}
