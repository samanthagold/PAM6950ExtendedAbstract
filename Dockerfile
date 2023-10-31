FROM rocker/r-ver:latest

SHELL ["/bin/bash", "-c"]

COPY ./ ./tugboat_dir

RUN source /etc/os-release && \
    R -e "install.packages('renv')"

RUN source /etc/os-release && \
    apt-get update && \
    apt-get install -y --no-install-recommends libxkbcommon-x11-0 \
    ca-certificates \
    lsb-release \
    file \
    git \
    libapparmor1 \
    libclang-dev \
    libcurl4-openssl-dev \
    libedit2 \
    libobjc4 \
    libssl-dev \
    libpq5 \
    psmisc \
    procps \
    python-setuptools \
    pwgen \
    sudo \
    wget \
    gdebi-core \
    libcairo2-dev \
    libxml2-dev \
    libgit2-dev \
    default-libmysqlclient-dev \
    libpq-dev \
    libsasl2-dev \
    libsqlite3-dev \
    libssh2-1-dev \
    libxtst6 \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libtiff5-dev \
    libjpeg-dev \
    unixodbc-dev \
    gdal-bin \
    lbzip2 \
    libfftw3-dev \
    libgdal-dev \
    libgeos-dev \
    libgsl0-dev \
    libgl1-mesa-dev \
    libglu1-mesa-dev \
    libhdf4-alt-dev \
    libhdf5-dev \
    libjq-dev \
    libproj-dev \
    libprotobuf-dev \
    libnetcdf-dev \
    libudunits2-dev \
    netcdf-bin \
    postgis \
    protobuf-compiler \
    sqlite3 \
    tk-dev \
    cmake \
    default-jdk \
    fonts-roboto \
    ghostscript \
    hugo \
    less \
    libbz2-dev \
    libglpk-dev \
    libgmp3-dev \
    libhunspell-dev \
    libicu-dev \
    liblzma-dev \
    libmagick++-dev \
    libopenmpi-dev \
    libpcre2-dev \
    libv8-dev \
    libxslt1-dev \
    libzmq3-dev \
    qpdf \
    texinfo \
    software-properties-common \
    vim \
    libpng-dev && \
    apt-get update

ENV RSTUDIO_VERSION="2023.09.1+494"
RUN source /etc/os-release && \
    apt-get update && \
    apt-get install -y --no-install-recommends ca-certificates \
    lsb-release \
    file \
    git \
    libapparmor1 \
    libclang-dev \
    libcurl4-openssl-dev \
    libedit2 \
    libobjc4 \
    libssl-dev \
    libpq5 \
    psmisc \
    procps \
    python-setuptools \
    pwgen \
    sudo \
    wget && \
    ARCH=$(dpkg --print-architecture) && \
    /rocker_scripts/install_s6init.sh && \
    DOWNLOAD_FILE=rstudio-server.deb && \
    wget "https://s3.amazonaws.com/rstudio-ide-build/server/${UBUNTU_CODENAME}/${ARCH}/rstudio-server-${RSTUDIO_VERSION/'+'/'-'}-${ARCH}.deb" -O "$DOWNLOAD_FILE" && \
    gdebi -n "$DOWNLOAD_FILE" && \
    rm "$DOWNLOAD_FILE" && \
    ln -fs /usr/lib/rstudio-server/bin/rstudio-server /usr/local/bin && \
    ln -fs /usr/lib/rstudio-server/bin/rserver /usr/local/bin && \
    rm -f /var/lib/rstudio-server/secure-cookie-key && \
    mkdir -p /etc/R && \
    R_BIN=$(which R) && \
    echo "rsession-which-r=${R_BIN}" >/etc/rstudio/rserver.conf && \
    echo "lock-type=advisory" >/etc/rstudio/file-locks && \
    cp /etc/rstudio/rserver.conf /etc/rstudio/disable_auth_rserver.conf && \
    echo "auth-none=1" >>/etc/rstudio/disable_auth_rserver.conf && \
    mkdir -p /etc/services.d/rstudio && \
    echo -e '#!/usr/bin/with-contenv bash\n\
## load /etc/environment vars first:\n\
for line in $( cat /etc/environment ) ; do export $line > /dev/null; done\n\
exec /usr/lib/rstudio-server/bin/rserver --server-daemonize 0' >/etc/services.d/rstudio/run && \
    echo -e '#!/bin/bash\n\
/usr/lib/rstudio-server/bin/rstudio-server stop' >/etc/services.d/rstudio/finish && \
    if [ -n "$CUDA_HOME" ]; then \
        sed -i '/^rsession-ld-library-path/d' /etc/rstudio/rserver.conf && \
        echo "rsession-ld-library-path=$LD_LIBRARY_PATH" >>/etc/rstudio/rserver.conf ; \
    fi && \
    echo -e '[*]\n\
log-level=warn\n\
logger-type=syslog' >/etc/rstudio/logging.conf && \
    /rocker_scripts/default_user.sh "rstudio" && \
    cp /rocker_scripts/init_set_env.sh /etc/cont-init.d/01_set_env && \
    cp /rocker_scripts/init_userconf.sh /etc/cont-init.d/02_userconf && \
    cp /rocker_scripts/pam-helper.sh /usr/lib/rstudio-server/bin/pam-helper && \
    rm -rf /var/lib/apt/lists/* && \
    echo -e "# /etc/rstudio/rsession.conf\nsession-default-working-dir=/tugboat_dir" >> /etc/rstudio/rsession.conf && \
    echo -e "\nInstall RStudio Server, done!"

EXPOSE 8787

ENV PANDOC_VERSION="default"
RUN rocker_scripts/install_pandoc.sh

ENV QUARTO_VERSION="default"
RUN rocker_scripts/install_quarto.sh

WORKDIR ./tugboat_dir

RUN source /etc/os-release && \
    R -e "if(file.exists('./renv.lock')) { lockfile <- renv::lockfile_read('./renv.lock'); exclude_pkgs <- c('base', 'boot', 'class', 'cluster', 'codetools', 'compiler', 'datasets', 'docopt', 'foreign', 'graphics', 'grDevices', 'grid', 'KernSmooth', 'lattice', 'littler', 'MASS', 'Matrix', 'methods', 'mgcv', 'nlme', 'nnet', 'parallel', 'rpart', 'spatial', 'splines', 'stats', 'stats4', 'survival', 'tcltk', 'tools', 'utils', 'renv'); updated_lockfile_pkgs <- lockfile[['Packages']][!names(lockfile[['Packages']]) %in% exclude_pkgs]; lockfile[['Packages']] <- updated_lockfile_pkgs; print(sort(names(lockfile[['Packages']]))); renv::lockfile_write(lockfile, './renv.lock') }" && \
    R -e "if(file.exists('./renv.lock')) { renv::restore(lockfile = './renv.lock', prompt = FALSE) }" && \
    R -e "renv::install(c('yaml'), prompt = FALSE, type = 'binary')" && \
    R -e "install <- function(package) { if (isFALSE(require(package, quietly = TRUE, character.only = TRUE))) { tryCatch({ renv::install(package, prompt = FALSE, type = 'binary') }, error = function(err) cat('Failed to install', package, '\n')) } }; r_deps <- renv::dependencies(); lapply(r_deps[['Package']], install)"

RUN chmod -R a+rwX /opt && \
    chmod -R a+rwX /srv && \
    chmod -R a+rwX /tugboat_dir

CMD ["/bin/bash"]