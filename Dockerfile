FROM fedora:42
COPY setup-os-fedora.sh /
RUN /setup-os-fedora.sh && rm /setup-os-fedora.sh
VOLUME /home
RUN groupadd jp && \
    useradd -c JP -d /home/jp -g jp -G wheel -s /bin/zsh jp
USER jp:jp
WORKDIR /home/jp
COPY --chown=jp:jp . ./dotfiles
RUN ./dotfiles/setup.sh
